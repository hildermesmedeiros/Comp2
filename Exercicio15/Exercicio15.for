      implicit none
      real*8,A(50,30),B(30,20),C(50,20)
      integer i,j,k,m,n,p      
      
      write(*,*)'Programa matriz x Matriz'
      write(*,*)'turma de compII'
      write(*,*)'Autor: Hildermes Jose Medeiros Filho'
      Write(*,*)'A(m,n), B(n,p)'
      write(*,*)'13/07/2015'

c     Leitura de m, n, p
      m=-1
      while(m.LE.0.OR.m.GT.50)Do
       write(*,*)'Entre com m:'
       read(*,*)m
      End While
      
      n=-1
      while(n.LE.0.OR.n.GT.30)Do
       write(*,*)'Entre com n:'
       read(*,*)n
      End While
      
      p=-1
      While(p.LE.0.OR.p.GT.20)Do
       write(*,*)'Entre com p:'
       read(*,*)p
      End While
c     Lendo A
      Do i=1,m
       Do j=1,n
        write(*,*) 'Entre com A(',i,',',j,')'
        read(*,*)A(i,j)
       End Do
      End Do
c     Lendo B
      Do i=1,n
       Do j=1,p
        write(*,*)'Entre com B(',i,',',j,')'
        read(*,*)B(i,j)
       End Do
      End do
      
c     escrevendo A na tela
      write(*,*)'Matriz A'
      Do i=1,m
      write(*,30)(A(i,j),j=1,n)
      End DO
 30   Format(50F13.3) 
 
c     escrevendo B na tela
      write(*,*)'Matriz B'
      Do i=1,n
      write(*,40)(B(i,j),j=1,p)
      End DO
 40   Format(50F13.3)

c     calculos
      Do i=1,m
       Do j=1,p   
        C(i,j)=0.d0
        Do k=1,n
         C(i,j)=C(i,j)+ A(i,k)*B(k,j)
        end Do
       End Do
      End Do
      Write(*,*)'A matriz C=A*B, eh:'
      write(*,*)'Matriz C'
      Do i=1,m
      write(*,50)(C(i,j),j=1,p)
      End DO
 50   Format(50F13.3)
      write(*,*)
      write(*,*)'Aperte uma tecla para fechar.'
      read(*,*)
      End  
        
           
            
       

               