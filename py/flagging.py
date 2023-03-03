"""
This file contains all necessary functions to create a DataFrame ready to use for
non-arms length transaction detection using statistical and heurstic methods.
"""

import re
import numpy as np
import pandas as pd

from scipy.stats import zscore
from sklearn.ensemble import IsolationForest
from sklearn.decomposition import PCA

SHORT_TERM_OWNER_THRESHOLD = 365 # 365 = 365 days or 1 year


def create_group_string(groups: tuple, sep: str) -> str:
    """
    Creates a string joined on a separator from the groups tuple.
    For the purpose of making column names and descriptions.
    Inputs:
        groups (tuple): the columns being used in groupby()
        sep (str): string to separate the groups with.
    Outputs:
        groups as a string joined by given separator
    """
    return sep.join(groups)


def outlier_taxonomy(df: pd.DataFrame, permut: tuple, groups: tuple):
    """
    Creates columns having to do with our chosen outlier taxonomy.
    Ex: Family sale, Home flip sale, Non-person sale, High price (raw and or sqft), etc.
    Inputs:
        df (pd.DataFrame): dataframe to create taxonomy on.
        permut (tuple): permutation of std deviations
        groups (tuple): columns to do grouping on.
                        Probably 'township' and 'class'.
    Ouputs:
        df (pd.DataFrame): dataframe with outlier taxonomy
    """

    df = check_days(df, SHORT_TERM_OWNER_THRESHOLD)

    df = pricing_info(df, permut, groups)

    df = outlier_type(df)
    df = outlier_flag(df)
    df = special_flag(df)

    return df


def iso_forest(df: pd.DataFrame,
               groups: tuple,
               columns: list,
               n_estimators: int = 1000,
               max_samples: int or float = .2) -> pd.DataFrame:
    """
    Runs an isolation forest model on our data for outlier detection.
    First does PCA, then, attaches township/class info, and then runs the
    IsoForest model with given parameters.
    Inputs:
        df (pd.DataFrame): dataframe with data for IsoForest
        groups (tuple): grouping for the data to input into the IsoForest
        columns (list): list with columns to run PCA/IsoForest on
        n_estimators (int): number of estimators in IsoForest
        max_samples(int or float): share of data to use as sample if float,
                                   number to use if int
    Outputs:
        df (pd.DataFrame): with 'sv_anomaly' column from IsoForest.
    """

    df.set_index('meta_sale_document_num', inplace=True)

    feed = pca(df, columns)

    feed.index = df.index
    for group in groups:
        feed[group] = df[group]

    isof = IsolationForest(n_estimators=n_estimators, max_samples=max_samples, bootstrap=True, random_state=42)
    df['sv_anomaly'] = isof.fit_predict(feed)

    df['sv_anomaly'] = np.select([(df['sv_anomaly'] == -1), (df['sv_anomaly'] == 1)],
                                          ['Outlier', 'Not Outlier'], default= 'Not Outlier')

    df.reset_index(inplace=True)

    return df


def pca(df:pd.DataFrame, columns: list) -> pd.DataFrame:
    """
    Runs PCA on data, selects compoents where explained variance > 1.
    Inputs:
        df (pd.DataFrame): dataframe to run PCA on.
        columns (list): columns of dataframe to run PCA on.
    Outputs:
        df (pd.DataFrame): dataframe of principal components
    """

    feed_data = df[columns]
    feed_data = feed_data.fillna(0)
    feed_data = feed_data.replace([np.inf, -np.inf], 0)

    pca = PCA(n_components = len(feed_data.columns))
    pc = pca.fit_transform(feed_data)

    cols = ['PC' + str(num) for num in range(len(feed_data.columns))]

    pc_df = pd.DataFrame(data = pc, 
                         columns = cols)
    take = len(pca.explained_variance_[pca.explained_variance_ > 1])

    df = pc_df[pc_df.columns[:take]]

    return df

def pricing_info(df: pd.DataFrame, permut: tuple, groups: tuple) -> pd.DataFrame:
    """
    Creates information about whether the price is an outlier, and its movement.
    Also fetches the sandard deviation for the record.
    pricing is whether it is a high/low outlier and whether it is a price swing.
    which_price is whether it is the raw price, price/sqft or both that are outliers.
    Inputs:
        df (pd.DataFrame): dataframe of sales
        permut (tuple): tuple of standard deviation boundaries.
                        Ex: (2,2) is 2 std away on both sides.
    Outputs:
        df (pd.DataFrame): dataframe with 3 extra columns of price info.
    """
    group_string = create_group_string(groups, '_')

    df = z_normalize(df, ['meta_sale_price', 'sv_price_per_sqft'])

    prices = [f'sv_price_per_sqft_deviation_{group_string}',
              f'sv_price_deviation_{group_string}', f'sv_cgdr_deviation_{group_string}']

    df[f'sv_price_deviation_{group_string}'] = df.groupby(list(groups), group_keys=False)['meta_sale_price'].apply(z_normalize_groupby)
    df[f'sv_price_per_sqft_deviation_{group_string}'] = df.groupby(list(groups), group_keys=False)['sv_price_per_sqft'].apply(z_normalize_groupby)
    df[f'sv_cgdr_deviation_{group_string}'] = df.groupby(list(groups), group_keys=False)['sv_cgdr'].apply(z_normalize_groupby)

    holds = get_thresh(df, prices, permut, groups)

    df['sv_pricing'] = df.apply(price_column, args=(holds, groups), axis=1)
    df['sv_which_price'] = df.apply(which_price, args=(holds, groups), axis=1)

    return df


def special_flag(df: pd.DataFrame) -> pd.DataFrame:
    """
    Creates column that checks whether there is a special flag for this record.
    Inputs:
        df (pd.DataFrame): dataframe to add flags onto
    Outputs:
        df (pd.DataFrame): dataframe with 'special_flags' column
    """
    cond = [(df['sv_name_match'] != 'No match'), (df['sv_short_owner'] == 'Short-term owner'),
            (df['sv_transaction_type'] == 'legal_entity-legal_entity')]
    labels = ['Family sale', 'Home flip sale', 'Non-person sale']

    df['sv_special_flags'] = np.select(cond, labels, default = 'Not special')

    return df


def which_price(row: pd.Series, thresholds: dict, groups: tuple) -> str:
    """
    Determines whether sale_price, price_per_sqft, or both are outliers,
    and returns a string resembling it.
    Inputs:
        thresholds (dict): dict of thresholds from get_thresh
    Outputs:
        value (str): string saying which of these are outliers.
    """
    value = 'Non-outlier'
    group_string = create_group_string(groups, '_')
    key = tuple(row[group] for group in groups)

    if thresholds.get(f'sv_price_deviation_{group_string}').get(key) and \
        thresholds.get(f'sv_price_per_sqft_deviation_{group_string}').get(key):
        s_std, *s_std_range = thresholds.get(f'sv_price_deviation_{group_string}').get(key)
        s_lower, s_upper = s_std_range
        sq_std, *sq_std_range = thresholds.get(f'sv_price_per_sqft_deviation_{group_string}').get(key)
        sq_lower, sq_upper = sq_std_range
        if not between_two_numbers(row[f'sv_price_deviation_{group_string}'], s_lower, s_upper) and \
            between_two_numbers(row[f'sv_price_per_sqft_deviation_{group_string}'], sq_lower, sq_upper):
            value = '(raw)'
        elif between_two_numbers(row[f'sv_price_deviation_{group_string}'], s_lower, s_upper) and \
            not between_two_numbers(row[f'sv_price_per_sqft_deviation_{group_string}'], sq_lower, sq_upper):
            value = '(sqft)'
        elif not between_two_numbers(row[f'sv_price_deviation_{group_string}'], s_lower, s_upper) and \
            not between_two_numbers(row[f'sv_price_per_sqft_deviation_{group_string}'], sq_lower, sq_upper):
            value = '(raw & sqft)'

    return value


def between_two_numbers(num: int or float, a: int or float, b: int or float) -> bool:
    return  a < num < b


def price_column(row: pd.Series, thresholds: dict, groups: tuple) -> str:
    """
    Determines whether the record is a high price outlier or a low price outlier.
    If the record is also a price change outlier, than add 'swing' to the string.
    Inputs:
        thresholds (dict): dict of standard deviation thresholds from get_thresh()
    Outputs:
        value (str): string showing what kind of price outlier the record is.
    """
    value = 'Not price outlier'
    price = False

    group_string = create_group_string(groups, '_')
    key = tuple(row[group] for group in groups)

    if thresholds.get(f'sv_price_deviation_{group_string}').get(key) and \
        thresholds.get(f'sv_price_per_sqft_deviation_{group_string}').get(key):

        s_std, *s_std_range = thresholds.get(f'sv_price_deviation_{group_string}').get(key)
        s_lower, s_upper = s_std_range

        sq_std, *sq_std_range = thresholds.get(f'sv_price_per_sqft_deviation_{group_string}').get(key)
        sq_lower, sq_upper = sq_std_range

        if row[f'sv_price_deviation_{group_string}'] > s_upper or\
            row[f'sv_price_per_sqft_deviation_{group_string}'] > sq_upper:
            value = 'High price'
            price = True
        elif row[f'sv_price_deviation_{group_string}'] < s_lower or\
            row[f'sv_price_per_sqft_deviation_{group_string}'] < sq_lower:
            value = 'Low price'
            price = True

        if price and pd.notnull(row[f'sv_cgdr_deviation_{group_string}']) and\
            thresholds.get(f'sv_cgdr_deviation_{group_string}').get(key):
            # not every combo will have pct change info so we need this check
            p_std, *p_std_range = thresholds.get(f'sv_cgdr_deviation_{group_string}').get(key)

            p_lower, p_upper = p_std_range
            if row['sv_price_movement'] == 'Away from mean' and \
                not between_two_numbers(row[f'sv_cgdr_deviation_{group_string}'], p_lower, p_upper):
                value += ' swing'

    return value


def create_stats(df: pd.DataFrame, groups: tuple) -> pd.DataFrame:
    """
    Create all statistical outlier measures.
    Inputs:
        df (pd.DataFrame): Dataframe to create statistics from
        groups (tuple): grouping for groupby. Usually 'township' and 'class'
    Outputs:
        df(pd.DataFrame): dataframe with statistical measures calculated.
    """

    df = price_sqft(df)
    df = grouping_mean(df, groups)
    df = deviation_dollars(df, groups)
    df = dup_stats(df, groups)
    df = transaction_days(df)
    df = percent_change(df)

    return df


def percent_change(df: pd.DataFrame) -> pd.DataFrame:
    """
    Generates CGR for all records. Requires that transaction_days() has already been run.
    Creates 'previous_price' column as intermediary to help calculate CGR.
    Calculate the compound growth rate where the previous transaction is the
    beginning value, the current price is the end value, and the number of periods
    is the number of days since the last transaction.
    This enables us to better compare percent change accross different time periods
    as opposed to pandas pct_change() function which does not account for time period.
    Helper for create_stats().
    Inputs:
        df (pd.DataFrame): datarame to create CGR on.
    Outputs:
        df (pd.DataFrame): dataframe with CGR statistic and previous_price column
    """

    df['sv_previous_price'] = df.sort_values('meta_sale_date').groupby(['meta_pin'])['meta_sale_price'].shift(axis=0)
    df['sv_cgdr'] = ((df['meta_sale_price'] / df['sv_previous_price']) ** (1 / df['sv_days_since_last_transaction'])) - 1

    return df


def dup_stats(df: pd.DataFrame, groups: tuple) -> pd.DataFrame:
    """
    Stats that can only be calculated for PINs occuring more than once, such as sale volatiltiy,
    and growth rates.
    Helper for create_stats().
    Inputs:
        df (pd.DataFrame): dataframe with sales data
        groups (tuple): for get_movement groups
    Outputs:mean
        df(pd.DataFrame): dataframe with sale counts and town_class movement columns.
    """
    dups = df[df.meta_pin.duplicated(keep=False)]
    dups = get_sale_counts(dups)
    dups = get_movement(dups, groups)

    df = pd.merge(df, dups, how='outer')

    return df


def price_sqft(df: pd.DataFrame) -> pd.DataFrame:
    """
    Creates price/sqft columns in DataFrame. Must contain 'sale_price',
    'sale_price_log10' and 'sqft' in the columns, where the first two names are
    self explanatory and 'sqft' is the properties square footage.
    Helper for create_stats().
    Inputs:
        df (pd.DataFrame): pandas dataframe with required columns.
    Outputs:
        df (pd.DataFrame): pandas dataframe with _per_sqft columns.
    """
    df['sv_price_per_sqft'] = df['meta_sale_price'] / df['char_bldg_sf']
    df['sv_price_per_sqft'].replace([np.inf, -np.inf], np.nan, inplace=True)

    return df


def deviation_dollars(df: pd.DataFrame, groups: tuple) -> pd.DataFrame:
    """
    Creates the deviation in dollars of this record from the mean
    sale_price and price_per_sqft for the groupby groups.
    Inputs:
        df (pd.DataFrame): dataframe to create deviations on
        groups (tuple): tuple of groups being grouped by
    Outputs:
        df (pd.DataFrame): dataframe with deviation columns
    """
    group_string = create_group_string(groups, '_')

    df[f'sv_deviation_{group_string}_mean_price'] = df['meta_sale_price'] - df[f'sv_mean_price_{group_string}']
    df[f'sv_deviation_{group_string}_mean_price_per_sqft'] = df['sv_price_per_sqft'] - df[f'sv_mean_price_per_sqft_{group_string}']

    return df


def grouping_mean(df: pd.DataFrame, groups: tuple) -> pd.DataFrame:
    """
    Gets sale_price mean by two groupings. Usually town + class.
    Helper for create_stats().
    Inputs:
        df (pd.DataFrame): dataframe with the grouping columns
        groups (tuple): tuple (len == 2) where each element is a column name to be grouped by.
    Outputs:
        df (pd.DataFrame): dataframe with grouped by mean column
    """
    group_string = create_group_string(groups, '_')

    group_mean = df.groupby(list(groups))['meta_sale_price'].mean()
    group_mean_sqft = df.groupby(list(groups))['sv_price_per_sqft'].mean()
    df.set_index(list(groups), inplace=True)
    df[f'sv_mean_price_{group_string}'] = group_mean
    df[f'sv_mean_price_per_sqft_{group_string}'] = group_mean_sqft

    df.reset_index(inplace=True)

    return df


def get_sale_counts(dups: pd.DataFrame) -> pd.DataFrame:
    """
    Calculates how many times transactions occured for a gieven property.
    Helper for dup_stats()
    Inputs:
        df (pd.DataFrame): pandsa dataframe
    """
    v_counts = dups.meta_pin.value_counts().reset_index().rename(columns={'index':'meta_pin', 'meta_pin':'sv_sale_dup_counts'})
    dups = pd.merge(dups, v_counts, how='outer')

    return dups


def get_movement(dups: pd.DataFrame, groups:tuple) -> pd.DataFrame:
    """
    Creates a coloumn that determines whether the price movement of the records is
    towards or away from the mean.
    Helper for dup_stats().
    Inputs:
        df (pd.DataFrame): duplicate records
        groups (tuple): groupby groups
    Outputs:
        df (pd.DataFrame): duplicate records with new column
    """
    group_string = create_group_string(groups, '_')

    dups[f'sv_deviation_{group_string}_mean_price_abs'] = abs(dups[f'sv_mean_price_{group_string}'] - dups['meta_sale_price'])

    temp = dups.sort_values('meta_sale_date').groupby(['meta_pin'])[f'sv_deviation_{group_string}_mean_price_abs'].shift()
    dups['sv_price_movement'] = dups[f'sv_deviation_{group_string}_mean_price_abs'].lt(temp).astype(float)
    dups['sv_price_movement'] = np.select([(dups['sv_price_movement'] == 0), (dups['sv_price_movement'] == 1)],
                                        ['Away from mean', 'Towards mean'], default='First sale')

    return dups


def transaction_days(df: pd.DataFrame) -> pd.DataFrame:
    """
    For each record, gets number of days since the last transaction.
    Inputs:
        df (pd.DataFrame): DataFrame with a sale_date column in datetime
    Outputs:
        df (pd.DataFrame): DataFrame with new column
    """

    df['sv_days_since_last_transaction'] = \
    df.sort_values('meta_sale_date').groupby('meta_pin')['meta_sale_date'].diff().apply(lambda x: x.days)

    return df


def check_days(df: pd.DataFrame, threshold: int) -> pd.DataFrame:
    """
    Creates a label of whether or not the transaction
    was only owned for a short term.
    If owned for less than the threshold, is a short term owner.
    Inputs:
        df (pd.DataFrame): dataframe to have short term owners checked
        threshold (int): the threshold fo being a short term owner
    Oututs:
        df (pd.DataFrame): datafrme with 'short_owner' column
    """
    df['sv_short_owner'] = np.select([(df['sv_days_since_last_transaction'] < threshold)],
                                  ['Short-term owner'], default = f'Over {threshold} days')

    return df


def get_thresh(df: pd.DataFrame, cols: list, permut: tuple, groups: tuple) -> dict:
    """
    Creates a nested dictionary where the top level key is a column
    and the 2nd-level key is a (township, class) combo.
    Ex: stds['sale_price'][76, 203]
    Needed in order to keep track of specific thresholds for each township/class combo.
    Theoretically each std should be 1(because of z_normalization), but in practical terms
    it is in a very very small range around 1, so using a uniform cutoff of 2 and -2
    loses us some precision.

    We also want to allow for some flexibility in how the thresholds are calculated;
    and this function allows for more flexbility in the event of future changes.
    Inputs:
        df (pd.DataFrame): Dataframe to create dictionary from.
        cols (list): list of columns to get standard deviations for.
        permut (tuple): standard deviation range for lower_limit and upper_limit
                        First term is how many stndard deviations away on the left
                        Second term is how many standard deviations away on the right.
    Outputs:
        stds (dict): nested dictionary of std deviations for all columns
                     from DataFrame.
    """
    stds = {}

    for col in cols:
        df[col] = df[col].astype(float)
        grouped = df.dropna(subset=list(groups) + [col]).groupby(list(groups))[col]
        lower_limit = grouped.mean() - (grouped.std(ddof=0) * permut[0])
        upper_limit = grouped.mean() + (grouped.std(ddof=0) * permut[1])
        std = grouped.std(ddof=0)
        lower_limit = lower_limit.to_dict()
        upper_limit = upper_limit.to_dict()
        std = std.to_dict()

        limits =  {x: (std.get(x, 0), lower_limit.get(x, 0), upper_limit.get(x, 0))
                for x in set(std).union(upper_limit, lower_limit)}
        stds[col] = limits

    return stds


def z_normalize(df: pd.DataFrame, columns: list) -> pd.DataFrame:
    """
    Do zscore normalization on given column set so that
    we can compare them apples to apples.
    Inputs:
        df (pd.DataFrame):
        columns (list): columsn to be normalized
    Outputs:
        df (pd.DataFrame): dataframe with given columns normalized
                           as 'column_name_zscore'
    """
    for col in columns:
        df['sv_' + col + '_deviation_county'] = zscore(df[col], nan_policy='omit')

    return df


def z_normalize_groupby(s: pd.Series):
    """
    Function used to z_normailize groups of records.
    Pandas stiches it back together into a complete column.
    Meant for groupby.apply().
    Inputs:
        s(pd.Series): grouped series from groupby.apply
    Ouputs:
        z_normalized series grouped by class and township
        that is then stiched into complete column by pandas
    """
    return zscore(s, nan_policy='omit')


def outlier_type(df: pd.DataFrame) -> pd.DataFrame:
    """
    Runs np.select that creates an outlier taxonomy.
    Inputs:
        df (pd.DataFrame): dataframe with necessary columns created from previous functions.
    Outputs:
        df (pd.DataFrame): dataframe with 'sv_outlier_type' column.
    """
    conditions = [
    (df['sv_short_owner'] == 'Short-term owner') & (df['sv_pricing'].str.contains('High')),
    (df['sv_name_match'] != 'No match') & (df['sv_pricing'].str.contains('High')),
    (df['sv_transaction_type'] == 'legal_entity-legal_entity') & (df['sv_pricing'].str.contains('High')),
    (df['sv_anomaly'] == 'Outlier') & (df['sv_pricing'].str.contains('High')),
    (df['sv_pricing'].str.contains('High price swing')),
    (df['sv_pricing'].str.contains('High')) & (df['sv_which_price'] == '(raw & sqft)'),
    (df['sv_pricing'].str.contains('High')) & (df['sv_which_price'] == '(raw)'),
    (df['sv_pricing'].str.contains('High')) & (df['sv_which_price'] == '(sqft)'),
    (df['sv_short_owner'] == 'Short-term owner') & (df['sv_pricing'].str.contains('Low')),
    (df['sv_name_match'] != 'No match') & (df['sv_pricing'].str.contains('Low')),
    (df['sv_transaction_type'] == 'legal_entity-legal_entity') & (df['sv_pricing'].str.contains('Low')),
    (df['sv_anomaly'] == 'Outlier') & (df['sv_pricing'].str.contains('Low')),
    (df['sv_pricing'].str.contains('Low price swing')),
    (df['sv_pricing'].str.contains('Low')) & (df['sv_which_price'] == '(raw & sqft)'),
    (df['sv_pricing'].str.contains('Low')) & (df['sv_which_price'] == '(raw)'),
    (df['sv_pricing'].str.contains('Low')) & (df['sv_which_price'] == '(sqft)')]

    labels = ['Home flip sale (high)', 'Family sale (high)',
              'Non-person sale (high)', 'Anomaly (high)',
              'High price swing',
              'High price (raw & sqft)', 'High price (raw)',
              'High price (sqft)',
              'Home flip sale (low)', 'Family sale (low)',
              'Non-person sale (low)', 'Anomaly (low)',
              'Low price swing',
              'Low price (raw & sqft)', 'Low price (raw)',
              'Low price (sqft)']

    df["sv_outlier_type"] = np.select(conditions, labels, default='Not outlier')

    return df


def outlier_flag(df: pd.DataFrame) -> pd.DataFrame:
    """
    Creates a flag that shows whether the record is an
    outlier (a special flag) according to our outlier taxonomy.
    Inputs:
        df (pd.DataFrame): dataframe to create outlier flag
    Outputs:
        df (pd.DataFrame): dataframe with 'is_outlier' column
    """

    df['sv_is_outlier'] = np.select([(df['sv_outlier_type'] == 'Not outlier')],
                                 ['Not outlier'], default='Outlier')

    return df


# STRING CLEANUP

"""
    An outline of our overall approach:

    Tries to create an identifier from the buyer/seller name.
    Our appraoch is to try to identify if it is a legal identify of some sort,
    such as a bank, construction company, trust, LLC, or other and
    return the string as-is with some formatting applied if so. We also combine some
    spellings/mispellings of big entities.

    If we can't identify the string as a legal entity we assume the string contains a person's name.
    We then process these strings to determine if the person is a trustee, successor,
    or a successor trustee from the fragements of the strings.
    Once we do this, we determine the best place tosplit the string in split_logic(),
    looking out for certain tokens. After we've determnined where to split
    the string we send the tokens to name_selector, where we attempt to select
    the last name of the string.

    We then create a column that tells us whether it's person, or a legal entity,
    as per our identification method that we used in get_id().

    Then we use the trustee, successor, or as successor trustee parts of
    the string we constructed earlier to determine the role of the buyer
    or seller in the transaction(trustee, successor, successor trustee).

    We then remove the trustee, successor, as successor trustee parts of the string
    from buyer/seller id.

    Finally we create a transaction_type column that is just what kind of entity it is
    with a dash between them.

    TODO: Process more string types:
        - If a name contains 'and', we split the string on it and take
          the token directly to the left. We could take a more sophisticated
          approach to determine if the last name in this case.
        - 'co-trustee' handling.
        -  Handle different name formats. Assume people use <FIRST M LAST>
           but sometimes its <LAST FIRST M> or other such formats.
        - Find trends in string cutoffs(some are cut off at 25, characters, others 25, etc)
          that could help use better process strings that are cutoff.
        - Cleanup/debug regex. This is a lot of dirty regex, and it is picking up
          some names that we don't want, or not correctly identifying every case that we do want.
          So it could use some work in some cases.
    """


entity_keywords = r"llc| ll$| l$|l l c|estate|training|construction|building|masonry|"\
                  r"apartments|plumbing|service|professional|roofing|advanced|office|"\
                  r"\blaw\b|loan|legal|production|woodwork|concepts|corp| company|"\
                  r" united|\binc\b|county|entertainment|community|heating|cooling"\
                  r"|partners|equity|indsutries|series|revitalization|collection|"\
                  r"agency|renovation|consulting|flippers|estates|\bthe \b|dept|"\
                  r"funding|opportunity|improvements|servicing|equities|sale|"\
                  r"judicial| in$|bank|\btrust\b|holding|investment|housing"\
                  r"|properties|limited|realty|development|capital|management"\
                  r"|developers|construction|rentals|group|investments|invest|"\
                  r"residences|enterprise|enterprises|ventures|remodeling|"\
                  r"specialists|homes|business|venture|restoration|renovations"\
                  r"|maintenance|ltd|real estate|builders|buyers|property|financial"\
                  r"|associates|consultants|international|acquisitions|credit|design"\
                  r"|homeownership|solutions|home|diversified|assets|family|land|"\
                  r"revocable|services|rehabbing|living|county of cook|fannie mae|"\
                  r"land|veteran|mortgage|savings|lp$"


def get_id(row: pd.Series, col: str) -> str:
    """
    Creates an ID from the buyer/seller name.

    Returns string as-is if identified as legal entity.
    Combined with other entities if its a common mispelling/cutoff.

    Attempts to identify last name if not a legal entity.

    Inputs:
        row: from apply()
        col (str): 'buyer' or 'seller'
    Outputs:
        id (str): string as-is if legal entity
                  identified last name if otherwise.
    """

    column = col + '_name'
    words = str(row[column]).lower()

    words = re.sub(r' amp ','', words)
    words = re.sub(' +', ' ', words)

    if words.isspace() or re.search(r'^[.]*$', words):
        id = 'Empty Name'
        return id

    if any(x in words for x in ['vt investment corpor', 'v t investment corp']):
        return 'vt investment corporation'

    if any(x in words for x in ['first integrity group inc', 'first integrity group in']):
        return 'first integrity group inc'

    if words in ['deutsche bank national tr']:
        return 'deutsche bank national trust company'

    if any(x in words for x in ['cirrus investment group l', 'cirrus investment group']):
        return 'cirrus investment group'

    if any(x in words for x in ['fannie mae aka federal na',
                                'fannie mae a k a federal', 'federal national mortgage']):
        return 'fannie mae'

    if any(x in words for x in ['the judicial sales corpor', 'judicial sales corp',
                                'judicial sales corporatio', 'judicial sale corp', 'the judicial sales corp']):
        return 'the judicial sales corporation'

    if any(x in words for x in ['jpmorgan chase bank n a', 'jpmorgan chase bank nati']):
        return 'jp morgan chase bank'

    if any(x in words for x in ['wells fargo bank na',  'wells fargo bank n a',
                                'wells fargo bank nationa',  'wells fargo bank n a a']):
        return 'wells fargo bank national'

    if any(x in words for x in ['bayview loan servicing l',  'bayview loan servicing ll']):
        return 'bayview loan servicing llc'

    if any(x in words for x in ['thr property illinois l', 'thr property illinois lp']):
        return 'thr property illinois lp'

    if any(x in words for x in ['ih3 property illinois lp', 'ih3 property illinois l']):
        return 'ih3 property illinois lp'

    if any(x in words for x in ['ih2 property illinois lp', 'ih2 property illinois l']):
        return 'ih2 property illinois lp'

    if any(x in words for x in ['secretary of housing and',
                                'the secretary of housing', 'secretary of housing ']):
        return 'secretary of housing and urban development'

    if any(x in words for x in ['secretary of veterans aff', 'the secretary of veterans']):
        return 'secretary of veterans affairs'

    if any(x in words for x in ['bank of america n a',
                                'bank of america na', 'bank of america national',]):
        return 'bank of america national'

    if any(x in words for x in ['us bank national association', 'u s bank national assoc',
                                'u s bank national associ', 'u s bank trust n a as', 'u s bank n a',
                                'us bank national associat', 'u s bank trust national']):
        return 'us bank national association'

    words = re.sub('suc t$|as succ t$|successor tr$|successor tru$|'\
                   'successor trus$|successor trust$|successor truste$|'\
                   'successor trustee$|successor t$|as successor t$',
                   'as successor trustee', words)
    words = re.sub('as t$|as s t$|as sole t$|as tr$|as tru$|as trus$|as trust$|'\
                   'as truste$|as trustee$|as trustee o$|as trustee of$|trustee of$|'\
                   'trustee of$|tr$|tru$|trus$|truste$|trustee$|, t|, tr|, tru|, trus|'\
                   ', trust|, truste',
                   'as trustee', words)
    words = re.sub('su$|suc$|succ$|succe$|succes$|success$|successo$|successor$|as s$|as su$|'\
                   'as suc$|as succ$|as succe$|as sucess$|as successo$|, s$|, su$|, suc$|, succ$|'\
                   ', succe$|, succes$|, success$|, successo$',
                   'as successor', words)

    if re.search(entity_keywords, words) or re.search(r'\d{4}|\d{3}', words) or \
        re.search('as trustee$|as successor$|as successor trustee$', words):
        id = words
        return id

    words = re.sub(' in$|indi$|indiv$|indivi$|indivi$|individ$|individu$|individua$|individual$'\
                   '|not i$|not ind$| ind$| inde$|indep$|indepe$|indepen$|independ$|independe$'\
                   '|independen$|independent$',
                   '', words)

    tokens = split_logic(words)

    id = name_selector(tokens)

    return id


def split_logic(words: str):
    """
    Given a cleaned string, determines where to split the string.
    Splits on 'and', variations of FKA/NKA/KNA if present, on spaces if not.
    Helper to get_id().
    Inputs:
        words (str): cleaned str from get_id
    Outputs:
        'Empty Name' if string is empty
        tokens (list): lsit of tokens in string from split
    """

    words = re.sub(' +', ' ', words)

    if words.isspace() or re.search(r'^[.]*$', words) or words == 'Empty Name':
        return 'Empty Name'

    words = re.sub(' as$| as $|as $','', words)

    _and = re.search(r'\b and\b|\b an$\b|\b a$\b|f k a|\bfka\b| n k a|\bnka\b|'\
                     r'\b aka\b|a k a|\b kna\b|k n a| f k$|n k$|a k$|\b not\b| married', words)

    if _and:
        tokens = words.split(_and.group())
        tokens = tokens[0].strip().split()
    else:
        tokens = words.split()

    return tokens


def name_selector(tokens) -> str:
    """
    Attempts to select the last name of a persons name based on number of tokens.
    Inputs:
        tokens: name to be identified
    Outputs:
        'Empty Name' if name is empty.
        id (str): identified last name
    """
    if tokens == 'Empty Name':
        return tokens
    # Ex: John Smith Jr
    if tokens[-1] in ['jr', 'sr', 'ii', 'iii', 'iv', 'v']:
        tokens = tokens[:-1]
    #Ex: John Smith
    if len(tokens) == 2:
        id = tokens[1]
    # John George Smith
    if len(tokens) == 3:
        id = tokens[2]
    # John George Theodore Smith
    else:
        id = tokens[-1]

    return id


def get_category(row: pd.Series, col: str) -> str:
    """
    Gets category buyer/seller id. legal_entity if in entity keywords,
    person if otherwise.
    Inputs:
        row: from pandas dataframe
        col (str): column to process. 'buyer' or 'seller'
    Outputs:
        category (str): category of buyer/seller id    
    """

    column = col + '_id'
    words = row[column]

    if re.search(entity_keywords, words):
        category = 'legal_entity'
    elif words == 'Empty Name':
        category = 'none'
    else:
        category = 'person'

    return category


def get_role(row: pd.Series, col: str) -> str:
    """
    Picks the role th person is playing in the transaction off of the
    buyer/seller_id. Meant for apply()
    Ex: 'as trustee', or 'as successor'
    Inputs:
        row: from pandas dataframe
        col (str): column to process. 'buyer' or 'seller'
    Outputs:
        roles(str): the role of the person n the transaction

    """
    role = None
    column = col + '_id'
    words = row[column]

    suc_trust = re.search(' as successor trustee' , words)
    suc = re.search(' as successor' , words)
    trust = re.search(' as trustee' , words)

    if suc_trust:
        role = suc_trust.group()

    if suc:
        role = suc.group()

    if trust:
        role = trust.group()

    return role


def clean_id(row: pd.Series, col: str) -> str:
    """
    Cleans id field after get_role() by removing role.
    Inputs:
        row: from padnas dataframe
        col (str): column to process. 'seller' or 'buyer'
    Outputs:
        words (str): seller/buyer id without role.
    """

    column = col + '_id'
    words = row[column]

    words = re.sub(r' as successor trustee|\b as successor\b| as trustee', '', words)
    words = re.sub(' as$| as $|as $','', words)

    if not (re.search(entity_keywords, words) or \
            re.search(r'\d{4}|\d{3}', words) or \
            len(words.split()) == 1):
        words = name_selector(split_logic(words))

    return words


def create_judicial_flag(df: pd.DataFrame) -> pd.DataFrame:
    """
    Creates a column that contains 1 if sold from a judicial corp
    and 0 otherwise. Mean for use with apply().
    Inputs:
        df (pd.DataFrame): dataframe to create flag on
    Outputs:
        df (pd.DataFrame): dataframe with 'sv_is_judicial_sale' column
    """

    df['sv_is_judicial_sale'] = np.select([(df['sv_seller_id'] == 'the judicial sale corporation') | (df['sv_seller_id'] == 'intercounty judicial sale')],
                                        ['1'], default = '0' )

    return df


def create_match_flag(row: pd.Series) -> str:
    """
    Creates a column that says whether the buyer/seller id match.
    Meant for apply().
    Inputs:
        row: from dataframe
    Outputs:
        value (str): whether the buyer and seller ID match
    """
    if row['buyer_id'] == row['seller_id'] and row['buyer_id'] != 'Empty Name':
        value = 'Buyer ID and Seller ID match'
    else:
        value = 'No match'

    return value


def create_name_match(row: pd.Series) -> str:
    """
    Creates a column that contains the actual string that was matched.
    Meant for apply().
    Inputs:
        row: from pandas dataframe
    Outputs:
        value (str or None): string match if applicable, None otherwise
    """
    if row['sv_buyer_id'] == row['sv_seller_id'] and row['sv_buyer_id'] != 'Empty Name':
        value = row['sv_seller_id']
    else:
        value = 'No match'

    return value


def string_processing(df: pd.DataFrame) -> pd.DataFrame:
    """
    Brings together all of the apply functions for string processing.
    Results in 7 additional columns.
    ID, category, and role for buyer and seller. As well as transaction category type
    for each record.
    Inputs:
        df (pd.dataFrame): dataframe with buyer/seller id columns.
    Ouputs:
        df(pd.DataFrame): dataframe with 7 new columns from apply functions
    """
    df.meta_sale_buyer_name = df.meta_sale_buyer_name.str.encode('ascii', 'ignore').str.decode('ascii')
    df.meta_sale_seller_name = df.meta_sale_seller_name.str.encode('ascii', 'ignore').str.decode('ascii')
    df.meta_sale_buyer_name = df.meta_sale_buyer_name.str.replace(r'[^a-zA-Z0-9\-]', ' ', regex=True).str.strip()
    df.meta_sale_seller_name = df.meta_sale_seller_name.str.replace(r'[^a-zA-Z0-9\-]', ' ', regex=True).str.strip()

    df['sv_buyer_id'] = df.apply(get_id, args=('meta_sale_buyer',), axis=1)
    df['sv_seller_id'] = df.apply(get_id, args=('meta_sale_seller',), axis=1)
    df['sv_buyer_category'] = df.apply(get_category, args=('sv_buyer',), axis=1)
    df['sv_seller_category'] = df.apply(get_category, args=('sv_seller',), axis=1)
    df['sv_buyer_id'] = df.apply(clean_id, args=('sv_buyer',), axis=1)
    df['sv_seller_id'] = df.apply(clean_id, args=('sv_seller',), axis=1)
    df['sv_transaction_type'] = df['sv_buyer_category'] + '-' + df['sv_seller_category']

    df = create_judicial_flag(df)
    df['sv_name_match'] = df.apply(create_name_match, axis=1)

    return df
