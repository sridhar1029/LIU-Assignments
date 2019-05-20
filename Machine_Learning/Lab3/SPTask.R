tanh <- function(z) {
  t1 = exp(z)
  t2 = exp(-z)
  a = (t1 - t2)/(t1 + t2)
  return(list(A, Z))
}

tanh_bac <- function(dA, z) {
  t = 1 - (tanh(z)^2)
  dZ = dA * t
  return(dZ)
}

initialize_parameters <- function(layer_dims) {
  set.seed(12345)
  parameters = list()
  L = length(layer_dims)
  for(i in 2:L){
    parameters[[paste('W', i)]] = matrix(rnorm((layer_dims[i]*layer_dims[i-1])),
                                       nrow = layer_dims[i], ncol = layer_dims[i-1])
    parameters[[paste('b', i)]] = matrix(rnorm(layer_dims[i]),
                                       nrow = layer_dims[i], ncol = 1)
  }
  return(parameters)
}

linear_forward <- function(A, W, b) {
  cache = list(A, W, b)
  Z = (W %*% A) + b
  return(list(Z, cache))
}

L_model_forwaard <- function(X, parameters) {
  A = X
  L = length(parameters)
  caches = list()
  for(i in 2:(L-1)){
    A_prev = A
    lin = linear_forward(A_prev, parameters[[paste('W', i)]], parameters[[paste('b', i)]])
    Z = lin[[1]]
    lin_cache = lin[[2]]
    act = tanh(Z)
    A = act[[1]]
    act_cache = act[[2]]
    caches[[i]] = list(lin_cache, act_cache)
  }
  lin = linear_forward(A, parameters[[paste('W', L)]], parameters[[paste('b', L)]])
  AO = lin[[1]]
  lin_cache = lin[[2]]
  caches[[L]] = lin_cache
  return(AO)
}

comp_error <- function(AO, Y) {
  n = length(AO)
  error = ((AO - Y)**2)/(2*n)
  return(error)
}

linear_backward <- function(dZ, cache){
  A_prev = cache; W = cache; b = cache;
  dW = (dZ %*% t(A_prev))/length(dZ)
  db =  sum(dZ)/(length(dZ))
  dA_prev = t(W) %*% dZ
  return(list(dA_prev, dW, db))
}

linear_activation_backward <- function(dA, cache){
  lin_cache = cache[[1]]; act_cache = cache[[2]]
  dZ = tanh_bac(dA, act_cache)
  lb = linear_backward(dZ, lin_cache)
  dA_prev = lb[[1]]
  dW = lb[[2]]
  db = lb[[3]]
  return(list(dA_prev, dW, db))
}

L_model_backward <- function(AO, Y, caches){
  grads = list()
  L = length(caches) + 1
  dAL = "dsds"
  current_cache = caches[[L]]
  gr = linear_activation_backward(dAL, current_cache)
  dA = gr[[1]]; dW = gr[[2]]; db = gr[[3]]
  grads[[L]] = list(dA, dW, db)
  for(l in rev(2:(L-1))){
    gr = linear_activation_backward(dA, caches[[l]])
    dA = gr[[1]]; dW = gr[[2]]; db = gr[[3]]
    grads[[l]] = list(dA, dW, db)
  }
  return(grads)
}

update_parameters <- function(parameters, grads, learn_rate){
  L = length(grads) + 1
  for(l in 2:L){
    dA = grads[[l]]; dW = grads[[l]]; db = grads[[l]]
    parameters[[paste('W', l)]] = parameters[[paste('W', l)]] - learn_rate*dW
    parameters[[paste('b', l)]] = parameters[[paste('b', l)]] - learn_rate*db
  }
  return(parameters)
}

L_layer_model <- function(X, Y, layer_dims, n_iter){
  cost = c()
  parameters = initialize_parameters(layer_dims)
  
  for(i in 1:n_iter){
    res = L_model_forwaard(X, parameters)
    AO = res[[1]]; caches = res[[2]]
    cost = comp_error(AO, Y)
    grads = L_model_backward(AO, caches)
    parameters = update_parameters(parameters, grads)
    costs = append(costs, cost)
  }
  print(costs)
  return(parameters)
}

X = as.matrix(runif(1000, 0, 10))
X = t(X)
Y = sin(X)
layer_dims = c(1, 10, 1)
n_iter = 50
initialize_parameters(layer_dims)
param = L_layer_model(X, Y, layer_dims, n_iter)
