doc_sims <- function(reps){
closing_time=0
customer=0
num_waited=0
mean_wait_time=0
for(q in 1:reps){
machine_time=0
max_machine_time=7*60
sim_time=0
next_arrival=0
next_service=0
wait_time=0
customer_out=0
in_service=0
i=1



while (machine_time<max_machine_time){
wait_time[i]=0
next_arrival[i]=rexp(1,1/10)
machine_time=machine_time+next_arrival[i]
if (machine_time<max_machine_time){
sim_time[i]=machine_time
next_service[i]=runif(1,min=5,max=20)
customer_out[i]=sim_time[i]+next_service[i]+wait_time[i]
for (j in 1:i){
  cust_out=customer_out[j] - machine_time
  if ( cust_out > 0){
   in_service[j]=1
  }
  else {
   in_service[j]=0
  }
}    
if (sum(in_service)>3){
  cust_out=customer_out - machine_time
  wait_time[i]=min(cust_out[cust_out>0])
}
else {
  
}
  
i=i+1
}
}

closing_time[q]=max(max(customer_out),420)
customer[q]=length(customer_out)
num_waited[q]=length(customer_out[wait_time>0])
if (num_waited[q]>0){
mean_wait_time[q]=mean(wait_time[wait_time>0])}
else{
  mean_wait_time[q]=0
}

}
if (reps==1){
  newlist<-list("Median Closing Time"=median(closing_time),"Median Customers"=median(customer),"Median Waited"=median(num_waited), "Meidan Wait Times"=median(mean_wait_time))
  return(newlist)
}
else {
  meanc=mean(closing_time)
  sdc=sd(closing_time)
  n=length(closing_time)
  sec=sdc/sqrt(n)
  E=qt(0.75,df=n-1)*sec
  confintc=meanc+c(-E,E)
  
  meanc=mean(customer)
  sdc=sd(customer)
  n=length(customer)
  sec=sdc/sqrt(n)
  E=qt(0.75,df=n-1)*sec
  confintcu=meanc+c(-E,E)
  
  meanc=mean(num_waited)
  sdc=sd(num_waited)
  n=length(num_waited)
  sec=sdc/sqrt(n)
  E=qt(0.75,df=n-1)*sec
  confintn=meanc+c(-E,E)
  
  meanc=mean(mean_wait_time)
  sdc=sd(mean_wait_time)
  n=length(mean_wait_time)
  sec=sdc/sqrt(n)
  E=qt(0.75,df=n-1)*sec
  confintm=meanc+c(-E,E)
  newlist<-list("Median Closing Time"=median(closing_time),"Closing Time Interval"=confintc,"Median Customers"=median(customer),"Customer Interval"=confintcu,"Median Waited"=median(num_waited),"Number Waited Interval"=confintn, "Meidan Wait Times"=median(mean_wait_time),"Wait Time Interval"=confintm)
  return(newlist)
 
}
}