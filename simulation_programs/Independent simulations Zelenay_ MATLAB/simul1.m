function [P_F,BIAS] = simul1(c,N,N_P,a,T,stand_dev,mean_QD)
m = length(stand_dev);
n = length(mean_QD);
QD = zeros(1,T);
PW = zeros(1,T);
%mean_PW = zeros(m,n);
P_F = zeros(m,n);       %forward price
BIAS = zeros(m,n);

x = 1/(c-1);

%simulation of demand and computation of forward price
for i=1:m
    for j=1:n
        for t=1:T
            QD(t) = normrnd(0,1)*stand_dev(1,i) + mean_QD(1,j);
            PW(t) = a*(QD(t)/N_P)^(c-1);
            if PW(t)<0
                PW(t)=0;
            end
        end
        %mean_PW(i,j) = mean(PW);
        %PR = 1.2*mean_PW;  
        
        a = 30*(N_P/mean(QD))^(c-1);
        
        PR = 1.2*mean(PW);
        cov1 = cov(PW.^x,PW);
        cov2 = cov(PW.^(x+1),PW);
        P_F(i,j) = mean(PW) - N_P/(N*c*(a^x))*( c*PR*cov1(1,2) - cov2(1,2) );
        BIAS(i,j) = (P_F(i,j) - mean(PW))/P_F(i,j);
    end
end

end