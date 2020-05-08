clc
clear
%initial states
c = 4;
N_R = 20;
N_P = 20;
a = 30*(N_P/100)^(c-1);
%A = 0.8/(c^2);
A = 0.8/(2^c);

%simulation
T = 10000;

%space
k = 40;
l1 = 75;
l2 = 125;

stand_dev = [1:k/40:k];
mean_QD = [l1:(l2-l1)/50:l2];

[X,Y] = meshgrid(mean_QD,stand_dev);


%computation of forward price
N = (N_P+N_R)/A;
[P_F,BIAS] = simul1(c,N,N_P,a,T,stand_dev,mean_QD);

%graph
surf(X,Y,BIAS)

xlabel('Expected demand')
ylabel('Demand standard deviation')
zlabel('Bias in forward power price percentage')