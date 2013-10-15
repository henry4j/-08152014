```bash
mahout cvb --help
Running on hadoop, using /usr/local/bin/hadoop and HADOOP_CONF_DIR=
MAHOUT-JOB: /workspace/mahout/mahout-examples-0.8-job.jar
13/09/05 20:45:41 WARN driver.MahoutDriver: No cvb.props found on classpath, will use command-line arguments only
Usage:                                                                          
 [--input <input> --output <output> --maxIter <maxIter> --convergenceDelta      
<convergenceDelta> --overwrite --num_topics <num_topics> --num_terms            
<num_terms> --doc_topic_smoothing <doc_topic_smoothing> --term_topic_smoothing  
<term_topic_smoothing> --dictionary <dictionary> --doc_topic_output             
<doc_topic_output> --topic_model_temp_dir <topic_model_temp_dir>                
--iteration_block_size <iteration_block_size> --random_seed <random_seed>       
--test_set_fraction <test_set_fraction> --num_train_threads <num_train_threads> 
--num_update_threads <num_update_threads> --persist_intermediate_doctopics      
<persist_intermediate_doctopics> --doc_topic_prior_path <doc_topic_prior_path>  
--max_doc_topic_iters <max_doc_topic_iters> --max_inf_doc_topic_iters           
<max_inf_doc_topic_iters> --num_reduce_tasks <num_reduce_tasks> --labeled_only  
<labeled_only> --backfill_perplexity --help --tempDir <tempDir> --startPhase    
<startPhase> --endPhase <endPhase>]                                             
Job-Specific Options:                                                           
  --input (-i) input                                                         Path to job input directory.  
  --output (-o) output                                                       The directory pathname for output.  
  --maxIter (-x) maxIter                                                     The maximum # of iterations. 
  --convergenceDelta (-cd) convergenceDelta                                  The convergence delta value 
  --overwrite (-ow)                                                          If present, overwrite the output directory
  --num_topics (-k) num_topics                                               # of topics to learn
  --num_terms (-nt) num_terms                                                Vocabulary size
  --doc_topic_smoothing (-a) doc_topic_smoothing                             Smoothing for document/topic distribution
  --term_topic_smoothing (-e) term_topic_smoothing                           Smoothing for topic/term distribution
  --dictionary (-dict) dictionary                                            Path to term-dictionary file(s)  
  --doc_topic_output (-dt) doc_topic_output                                  Output path for the training doc/topic distribution
  --topic_model_temp_dir (-mt) topic_model_temp_dir                          Path to intermediate model path 
                                                                             (useful for restarting) 
  --iteration_block_size (-block) iteration_block_size                       # of iterations per perplexity check
  --test_set_fraction (-tf) test_set_fraction                                Fraction of data to hold out for testing  
  --max_doc_topic_iters (-mipd) max_doc_topic_iters                          max # of iterations per doc for p(topic|doc) learning 
? --persist_intermediate_doctopics (-pidt) persist_intermediate_doctopics    persist and update intermediate p(topic|doc)
? --doc_topic_prior_path (-dtp) doc_topic_prior_path                         path to prior values of p(topic|doc) matrix
? --max_inf_doc_topic_iters (-int) max_inf_doc_topic_iters                   max # of iterations per doc for p(topic|doc) inference
? --labeled_only (-ol) labeled_only                                          only use docs with non-null doc/topic priors
  --backfill_perplexity                                                      enable back-filling of missing perplexity values
```
