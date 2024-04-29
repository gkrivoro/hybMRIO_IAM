# hybMRIO_IAM
Downscaling NGFS IAM final energy series to finer sectoral level using EXIOBASE.

in the order of what needs to be run

0) download requisite data, including EU KLEMS and EXIOBASE.
1) run exiobase_run_v8.R. First, change working directory and paths to EXIOBASE
    this creates a timestamped output under the run directory, where all subsequent analysis will be place
  (2* optional) decompose.R can give you a series by series decomposition of the output
3) klems_estimation_v3.R will give you share parameters for the back of the envelope output.
4) output_klems_v3_cluster.R will give you various iterations of output with various EOS estimates. 


