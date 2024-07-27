import pandas as pd

class Data: 
    """
    Process csv data using pandas by extracting selected columns and
    merging selected columns. 

    Attributes: 
        path (str): path to csv file 
        cols_to_extract (dict): dictionary mapping the raw column name 
        to the desired column name
        cols_to_merge (dict): dictionary mapping the desired name to a list
        of raw column names to merge 
    """
    def __init__(self, path: str, cols_to_extract: dict, cols_to_merge: dict):
        self.path = path
        self.cols_to_extract = cols_to_extract
        self.cols_to_merge = cols_to_merge
    
    def data_read_csv(self):
        if (not self.path.endswith(".csv")):
            raise Exception("Data must be in a csv file")
        return pd.read_csv(self.path)
    
    def data_extract_cols(self, df):
        """
        Extract columns from df and rename columns according to 
        self.cols_to_extract values. If cols to extract is None, 
        return empty df. 

        Args: 
            df (dataframe): dataframe to extract from
        
        Returns: 
            dataframe: dataframe of extracted columns 
        """
        # todo add check for if specified cols are not in df
        extracted = pd.DataFrame()
        if (not self.cols_to_extract == None):
            old_cols = list(self.cols_to_extract.keys())
            extracted = df[old_cols].rename(columns=self.cols_to_extract)
        return extracted
    
    def data_merge_cols(self, df):
        """
        Assuming all data is numeric, merges multiple columns into one 
        by adding across columns. Columns to merge are specifed by 
        self.cols_to_merge values and column names are specified by keys. 

        Returns empty df if there self.cols_to_merge is None (no cols to merge).

        Args: 
            df (dataframe): dataframe to extract from 
        
        Returns: 
            dataframe: dataframe containing merged columns
        """
        merged = dict()
        old_cols = self.cols_to_merge
        if (not old_cols == None): 
            for merge in old_cols:
                merged[merge] = df[list(old_cols[merge])].astype("int32").sum(1)
        return pd.DataFrame(merged) 

    def __str__(self):
        return f"Path: {self.path}\nCols to extract: {self.cols_to_extract}\nCols to merge: {self.cols_to_merge}"