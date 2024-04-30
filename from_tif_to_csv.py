import rasterio
import numpy as np
import pandas as pd
import os
import datetime

# We will delete them later to get only the date since we note that the file 
# name is for example 'chirps-v2.0.1981.01-02.tif'
# therefore, we need to trim the following header and extension to get the date
# file start with
file_header = 'chirps-v2.0.'
# file extension
file_extension = '.tif'

def sort_tif_files(name: str):
    date =  name.replace(file_header, '').replace(file_extension, '')
    key = datetime.datetime.strptime(date, '%Y.%m')
    return key

def extract_to_csv(working_dir: str, table_columns: list, coordinates: list):
    '''
    extract to csv file from a list of coordinates
    :param str workding_dir working directory containing tif files
    :param list table_columns columns name in a list
    :param list coordinates a list of xy coordinates to extract data
    '''
    # os.listdir return a list of files in the directory, then use list comprehension to
    # get only the file with .tif extension. After this line, files will contain only
    # a list of .tif files
    tifs = [file for file in os.listdir(working_dir) if file.endswith(file_extension) == True]
    # sort tif file according to date
    tifs.sort(key = sort_tif_files)
    # only to index our output file, we will have the output file such as Rainfall_0.csv,
    # Rainfall_1.csv, etc...
    file_index = 0
    # loop through the list of coordinates
    for coordinate in coordinates:
        print(coordinate)
        table = coordinate_to_csv(working_dir, tifs, table_columns, coordinate)
        # convert to csv one table by one
        table.to_csv(output_file_name + '_' + str(file_index) + '.csv')
        file_index += 1

def coordinate_to_csv(working_dir: str, tifs: list, table_columns: list, xy_coordinate: tuple):
    '''
    extract the corresponding value at the xy_coordinate
    :param str working_dir directory containing the tif files
    :param list table_columns columns name in a list
    :param tuple xy coordinate
    :return table
    '''
    # because each tif file is a date, therefore, the number of tif file is the number_of_dates
    # we use len to get the size of the list tifs, then we know how many dates we put inside the 
    # table, or the number of rows
    number_of_dates = len(tifs)
    # Create a table with rows, and columns with the below names, reserve one more row for the title
    table = pd.DataFrame(0, index = np.arange(1, number_of_dates + 1), columns = table_columns)

    table_row = 0
    for tif in tifs:
        print(tif)
        # here row = 1, since the 0 location is for the title, in this case
        # they are 'Date' and 'Rainfall'
        table_row = table_row + 1

        # raw dataset
        raw_dataset = rasterio.open(working_dir + tif)
        
        print(raw_dataset)
        # get the x, y location
        x, y = xy_coordinate

        # get row and column for our coordinate from the dataset
        row, col = raw_dataset.index(x, y)
        print(f"row = {row}, col={col}")
        # read the data set
        climate_data = raw_dataset.read(1)
        print(climate_data)
        # Now, we trim off the file_header and the file_extension, to get the
        # date, after that, we copy the date as a string to the table
        # we replace both the header and the extension with white space
        # then 'chirps-v2.0.1981.01-02.tif' --> '1981.01-02'
        date = tif.replace(file_header, '').replace(file_extension, '')

        # After removing header and extension, we copy the date as a string to
        # the table
        table[table_columns[0]].loc[table_row] = date

        # And we copy the value to the corresponding 'Rainfall' row
        table[table_columns[1]].loc[table_row] = climate_data[row, col]
        
        raw_dataset.close()

    return table

def read_coordinates(file_name: str, latitude_label: str, longitude_label: str):
    table = pd.read_excel(file_name)
    df = pd.DataFrame(table, columns = [latitude_label, longitude_label])
    df = df.reset_index() # make sure indexes pair with number of rows

    # define an emtpy list
    coordinates = []
    for _, row in df.iterrows():
        coordinates.append((row[latitude_label], row[longitude_label]))

    return coordinates

if __name__ == '__main__':
    # Directory that has the files
    working_dir = '

    xy_coordinates = read_coordinates(working_dir + 'gps-district.xlsx', 'hh_lat', 'hh_long')
    #xy_coordinates = [(21.21, -31.11)]
    # csv output name
    output_file_name = 'Rainfall'

    table_columns = ['Date', 'Rainfall']

    extract_to_csv(working_dir, table_columns, xy_coordinates)
