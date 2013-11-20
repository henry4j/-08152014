```bash
$ mahout seq2sparse --help
Running on hadoop, using /usr/local/bin/hadoop and HADOOP_CONF_DIR=
MAHOUT-JOB: /workspace/mahout/mahout-examples-0.8-job.jar
Usage:                                                                          
 [--minSupport <minSupport> --analyzerName <analyzerName> --chunkSize           
<chunkSize> --output <output> --input <input> --minDF <minDF> --maxDFSigma      
<maxDFSigma> --maxDFPercent <maxDFPercent> --weight <weight> --norm <norm>      
--minLLR <minLLR> --numReducers <numReducers> --maxNGramSize <ngramSize>        
--overwrite --help --sequentialAccessVector --namedVector --logNormalize]       
Options                                                                         
  --minSupport (-s) minSupport        (Optional) Minimum Support. Default Value: 2                                  
  --analyzerName (-a) analyzerName    The class name of the analyzer            
  --chunkSize (-chunk) chunkSize      The chunkSize in MegaBytes. 100-10000 MB  
  --output (-o) output                The directory pathname for output.        
  --input (-i) input                  Path to job input directory.              
  --minDF (-md) minDF                 The minimum document frequency. Default is 1                                      
  --maxDFSigma (-xs) maxDFSigma       What portion of the tf (tf-idf) vectors to be used, 
                                      expressed in times the standard deviation (sigma) 
                                      of the document frequencies of these vectors.
                                      Can be used to remove really high frequency terms. 
                                      Expressed as a double value. Good value to be specified is 3.0. 
                                      In case the value is less than 0 no vectors will be filtered out. 
                                      Default is -1.0.  Overrides maxDFPercent             
  --maxDFPercent (-x) maxDFPercent    The max percentage of docs for the DF.    
                                      Can be used to remove really high frequency terms. 
                                      Expressed as an integer between 0 and 100. Default is 99.
                                      If maxDFSigma is also set, it will override this value.                               
  --weight (-wt) weight               The kind of weight to use. Currently TF or TFIDF                                  
  --norm (-n) norm                    The norm to use, expressed as either a    
                                      float or "INF" if you want to use the Infinite norm.
                                      Must be greater or equal to 0.  The default is not to normalize    
  --minLLR (-ml) minLLR               (Optional) The minimum Log Likelihood Ratio (Float)  Default is 1.0              
  --numReducers (-nr) numReducers     (Optional) Number of reduce tasks. Default Value: 1                          
  --maxNGramSize (-ng) ngramSize      (Optional) The maximum size of ngrams to create
                                      (2 = bigrams, 3 = trigrams, etc) Default Value:1                           
  --overwrite (-ow)                   If set, overwrite the output directory    
  --sequentialAccessVector (-seq)     (Optional) Whether output vectors should  
                                      be SequentialAccessVectors. If set true else false                                
  --namedVector (-nv)                 (Optional) Whether output vectors should  
                                      be NamedVectors. If set true else false   
  --logNormalize (-lnorm)             (Optional) Whether output vectors should  
                                      be logNormalize. If set true else false   
```
