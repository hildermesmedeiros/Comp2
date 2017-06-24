      implicit none
      Real*8 x,total,med,x_soma
      Integer n,i
      
      
      n=-1
      while(n.lt.0)do
       write(*,*)'Entre com numero de termos'
       read(*,*)n
      End while
      
      total=0.d0
      x_soma=0.d0
      Do i=1,n
       Write(*,*)'Entre com x',i
       Read(*,*)x
       x_soma=x_soma+DABS(x)
       total=total+1.d0
      End DO
      
      med=x_soma/total
      
      Write(*,*)'A media dos valores absolutes de x eh:'
      Write(*,*)med
      Write(*,*)
      write(*,*)'Aperte uma teclar para fechar'
      read(*,*)
      End 
       
       