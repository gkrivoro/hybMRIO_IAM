# hybMRIO_IAM
This project contains the code underlying the paper "Long-Run Sectoral Transition Risk using a Hybrid MRIO/IAM Approach" (SSRN)[https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4811268].

The methodology represents a flexible bybrid-LCA-style methodology to downsclae IAM sectoral series from NGFS scenarios [https://www.ngfs.net/ngfs-scenarios-portal/]. This method produces final energy projections at a finer economic sectoral granularity. These series are then translated to output quantities with key elasticity parameters estimated using IEA or Eurostat energy balances paired with EU-KLEMS data. 

Using the example of the MESSAGEix-GLOBIOM, REMIND-MaGPIE, and GCAM IAMs and EXIOBASE, we generate projections under various transition scenarios across 50 countries and 40 sectors, and identify non-energy sectors with substantial transition downside risks along with sectors showcasing positive economic growth.

The scripts provided are the following:

1) immport_IAM.R. This script essentially prepares the IAM energy data by cleaning, filtering, and formatting it for use in subsequent analyses. It focuses on specific scenarios, regions, variables, and models relevant to the project's scope.
2) exiobase_run.R This script is central to the project, as it combines data from EXIOBASE and IAM models to create detailed energy use projections for various sectors and scenarios. It performs complex calculations to link different data sources and produce final energy series that can be used for further analysis. The output are stored in a _run_ folder which will be used to store all other results
3) decompose.R This script focuses on decomposing the energy use projections generated in the previous step. It breaks down the energy use into different components, allowing for a more detailed analysis of what's driving the changes in energy use across different sectors, countries, and scenarios.
4) eurostat_df/iea_df.R. These scripts are used to clean and analyze energy balances and energy prices datasets. Depending on your institution having access to IEA data (license required), you can use the Eurostat energy balance dataset (open access).
5) klems_estimation.R This R script calculates elasticities of substitution and input shares for capital, labor, and energy using EU-KLEMS data.
6) output_euklems.R This R script computes back-of-the-envelope calculations for projections of output quantities using clustered standard deviations from EU-KLEMS data.

