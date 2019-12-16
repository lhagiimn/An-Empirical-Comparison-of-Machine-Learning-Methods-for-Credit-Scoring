
MLP=function(training, test) {
	
	test=test[,match(names(training), names(test))]
	
	for (i in 1:length(training[1,])) {
		max_val = max(training[,i], test[,i])
		min_val = min(training[,i], test[,i])
		
		training[,i] = (training[,i] - min_val)/(max_val-min_val)
		test[,i] = (test[,i] - min_val)/(max_val-min_val)
	}

	result=list()
	
	trainingx=dummy.data.frame(training[,-match('LATE60', names(training))])
	trainingx=data.matrix(trainingx)
	y1=ifelse(training$LATE60==1, 1, 0)
	y2=ifelse(training$LATE60==0, 1, 0)
	trainingy=data.frame(y1, y2)
	trainingy=data.matrix(trainingy)

	testx=dummy.data.frame(test[,-match('LATE60', names(test))])
	testx=data.matrix(testx)
	testy=data.matrix(test$LATE60)

	model <- keras_model_sequential()
	model %>%
	layer_dense(units = 8, input_shape = ncol(trainingx)) %>%
	layer_activation('relu') %>%
	layer_dense(units = 2) %>%
	layer_activation('softmax')

	model %>% compile(
	   loss = 'binary_crossentropy',
	   optimizer = optimizer_adam( lr= 0.0001 , decay = 1e-6 ),  
	   metrics = c('accuracy')
	 )

	track = model %>% fit(trainingx, trainingy, epochs = 1000, batch_size = 32,
				   callbacks = callback_early_stopping(patience = 25, monitor = 'val_loss'),
				   validation_split = 0.2
	)
	
	fit_test <- model %>% predict(testx, batch_size = 32)
	fit_training <- model %>% predict(trainingx, batch_size = 32)

	result[[1]] <- fit_test
	result[[2]] <- fit_training
	
	return(result)
}


DMLP=function(training, test) {
	
	test=test[,match(names(training), names(test))]
	
	for (i in 1:length(training[1,])) {
		max_val = max(training[,i], test[,i])
		min_val = min(training[,i], test[,i])
		
		training[,i] = (training[,i] - min_val)/(max_val-min_val)
		test[,i] = (test[,i] - min_val)/(max_val-min_val)
	}

	result=list()
	
	trainingx=dummy.data.frame(training[,-match('LATE60', names(training))])
	trainingx=data.matrix(trainingx)
	y1=ifelse(training$LATE60==1, 1, 0)
	y2=ifelse(training$LATE60==0, 1, 0)
	trainingy=data.frame(y1, y2)
	trainingy=data.matrix(trainingy)

	testx=dummy.data.frame(test[,-match('LATE60', names(test))])
	testx=data.matrix(testx)
	testy=data.matrix(test$LATE60)

	model <- keras_model_sequential()
	model %>%
	layer_dense(units = 8, input_shape = ncol(trainingx)) %>%
	layer_activation('relu') %>%
	layer_dense(units = 8) %>%
	layer_activation('relu') %>%
	layer_dense(units = 8) %>%
	layer_activation('relu') %>%
	layer_dense(units = 2) %>%
	layer_activation('softmax')

	model %>% compile(
	   loss = 'binary_crossentropy',
	   optimizer = optimizer_adam( lr= 0.0001 , decay = 1e-6 ),  
	   metrics = c('accuracy')
	 )

	track = model %>% fit(trainingx, trainingy, epochs = 1000, batch_size = 32,
				   callbacks = callback_early_stopping(patience = 25, monitor = 'val_loss'),
				   validation_split = 0.2
	)
	
	fit_test <- model %>% predict(testx, batch_size = 32)
	fit_training <- model %>% predict(trainingx, batch_size = 32)

	result[[1]] <- fit_test
	result[[2]] <- fit_training
	
	return(result)
}

MDMLP=function(training, test) {
	
	test=test[,match(names(training), names(test))]
	
	for (i in 1:length(training[1,])) {
		max_val = max(training[,i], test[,i])
		min_val = min(training[,i], test[,i])
		
		training[,i] = (training[,i] - min_val)/(max_val-min_val)
		test[,i] = (test[,i] - min_val)/(max_val-min_val)
	}

	result=list()
	
	trainingx=dummy.data.frame(training[,-match('LATE60', names(training))])
	trainingx=data.matrix(trainingx)
	y1=ifelse(training$LATE60==1, 1, 0)
	y2=ifelse(training$LATE60==0, 1, 0)
	trainingy=data.frame(y1, y2)
	trainingy=data.matrix(trainingy)

	testx=dummy.data.frame(test[,-match('LATE60', names(test))])
	testx=data.matrix(testx)
	testy=data.matrix(test$LATE60)

	model <- keras_model_sequential()
	model %>%
	layer_dense(units = 8, input_shape = ncol(trainingx)) %>%
	layer_activation('relu') %>%
	layer_dense(units = 8) %>%
	layer_activation('relu') %>%
	layer_dense(units = 8) %>%
	layer_activation('relu') %>%
	layer_dense(units = 8) %>%
	layer_activation('relu') %>%
	layer_dense(units = 8) %>%
	layer_activation('relu') %>%
	layer_dense(units = 2) %>%
	layer_activation('softmax')

	model %>% compile(
	   loss = 'binary_crossentropy',
	   optimizer = optimizer_adam( lr= 0.0001 , decay = 1e-6 ),  
	   metrics = c('accuracy')
	 )

	track = model %>% fit(trainingx, trainingy, epochs = 1000, batch_size = 32,
				   callbacks = callback_early_stopping(patience = 25, monitor = 'val_loss'),
				   validation_split = 0.2
	)
	
	fit_test <- model %>% predict(testx, batch_size = 32)
	fit_training <- model %>% predict(trainingx, batch_size = 32)

	result[[1]] <- fit_test
	result[[2]] <- fit_training
	
	return(result)
}



modelling = function (training, test) {
	
	mlp_model_relu =list()
	deep_mlp_model_relu = list()
	deeper_mlp_model_relu = list()

	for (i in 1:10) {
		mlp_model_relu[[i]] = MLP(training, test)
		deep_mlp_model_relu[[i]] = DMLP(training, test)
		deeper_mlp_model_relu[[i]] = DMLP(training, test)
	}
	
	result=list()
	
	result$mlp_model_relu=mlp_model_relu
	result$deep_mlp_model_relu=deep_mlp_model_relu
	result$deeper_mlp_model_relu=deeper_mlp_model_relu
	
	return(result)
}


	
		
### scf results ### 
SCF_result_relu = modelling(training, test)

file.to.save <- ( 'SCF_result_relu.rda' )
		save(SCF_result_relu, file = file.to.save )

SCF_result_relu = modelling(training.bonf, test.bonf)

file.to.save <- ( 'SCF_result_relu.rda' )
		save(SCF_result_relu, file = file.to.save )

