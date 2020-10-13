# barcode-competitions
R code and configs for plotting the results of competitions between barcoded bacterial strains

This code expects the results of a competition experiment between barcoded bacteria, which consists of a set of data files each with two columns - the barcode sequences and the counts of each barcode. Three configurations files are also required: (1) a two-column barcodes.txt file with the barcode _names_ associated with each barcode sequence; (2) a four-column sample_layout.txt file with the sample name, replicate, time point, and corresponding data file; and (3) a three column barcode_layout.txt file with the sample name, the barcode, and the corresponding strain. 
