The Makefiles for our running is present in 
prose/Transformation.Text/ (Flash Fill) and
prose/Extraction.Text/ (FlashExtract)

The specifications for these tests are too much data to include in the
submission.  These specs can be regenerated through running
make generate-io-specs (Flash Fill specs) and
make generate-extraction-specs (FlashExtract specs)
from the program directory.

Furthermore, the rest of the prose directory was too big to be included in
the submission.  By placing these files in their locations in a downloaded
prose direcotry ( https://github.com/Microsoft/prose ), and running
make generate-data on both the Makefiles, with the generated io specs
in this folder, will regenerate the data, and place the generated data
in generated_data/data.csv.
