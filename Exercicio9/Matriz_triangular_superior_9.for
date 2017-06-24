c     Primeira linha de comando a ser digitada, sempre 
      implicit None
c         Declaracao de Variaveis
      real*8 A(50,50)
c     n, numero de elementos que tem a matrix A(n,n)
      Integer n,soma,VouF
c     inteiro para receber o iostat,<0 eh final de arquivo, 0 significa dado correto, e >0 eh um erro
c     o problema eh que o valor do erro depende do tipo de sistema, cada fabricante usa o seu
c     exemplo: http://www.msg.ucsf.edu/local/programs/IBM_Compilers/Fortran/html/pgs/lr76.htm
      integer u,o
c     Linhas(L) e colunas(C)           
      integer L,C      
c     Formats usados      
      character fmt*100,fmt_l*2,fmt_c*2 
      Write(*,*)'Programa que verifica se a matriz eh triangular superio
     &r'
      Write(*,*)'A(i,j)=0, para todo i>j'
      write(*,*)'Data:30/06/2015'
      write(*,*)'Autor: Hildermes Jose Medeiros Filho'
      Write(*,*)
c     Lendo n
      n=-1
      Write(*,*) 'A(n,n)'
      While(n.LE.0.OR.n.GT.50)Do
       Write(*,*)'Entre com N'      
       Read(*,*,iostat=u) n
c      se u>0 erro. n continua sendo -1 e volta a perguntar.       
       if(u.gt.0)then
        Write(*,*)'Erro, n precisa ser um inteiro'
       end if     
      End While
          write(*,*)
      write(*,*) 'Entre com A(i,j):'
c     dois loopins de do, se A(i,j) der erro, volta para dentro do loopin de linha, ou seja: pede novamente o termo A(i,j) que estava sendo digitado!      
      Do L=1,n
       if(L.LT.10)then
        write(fmt_L,*)'I1'
       elseif(L.LT.100)then
        write(fmt_L,*)'I2'
       else
        write(fmt_L,*)'I3'
       Endif
c      controle do iostat, se der erro vai voltar.
c     se receber o iostat de erro,ele vai para fora do loopin e imprime a msg de erro e a linha tem que ser redigitada  

c      Looping para a digitar a matriz termo a termo.
       Do c=1,n
  210  if(o.GT.0)then
        write(*,*)'A(i,j)=Numero real.Digite novamente.
     &'
       endif   
        if(c.LT.10)then
         write(fmt_c,*)'I1'
         fmt='(A2,'//fmt_l//',A1,'//fmt_c//',A1)'
         write(*,fmt)'A(',l,',',c,')'
         read(*,*,err=210,iostat=o) A(l,c)     
        else if(c.LT.100)then
         write(fmt_c,*)'I2'
         fmt='(A2,'//fmt_l//',A1,'//fmt_c//',A1)'
         write(*,fmt)'A(',l,',',c,')'
         read(*,*,err=210,IOSTAT=o) A(l,c)
c       poderia resetar o loop com err, mas nao saberia o que esta acontecendo.          
        end if
       End Do         
      End Do 
c     Apartir daqui processa os dados de entrada
c     Dois loopins de DO, se a linha maior que a coluna e o termo A(i,j) igual a zero, conto quantas ocorrencias.
c     Se Linha Linha maior que a coluna e o termo A(i,j) correspondente for diferente de 0, retorna falso, e conto ocorrencias de falso.
      Do l=1,n
       soma=0.d0
       VouF=-1.d0
       Do c=1,n
        if(l.gt.c.and.A(l,c).EQ.0.d0)then
         soma=soma+1
         VouF=1
        else if(l.gt.c.and.A(l,c).NE.0.d0)then
         VouF=0
        else
         VouF=VouF 
        endif 
       end do  
      End Do
c     se tiver falso, nao eh, simples.      
      if(VouF.LT.1)then
       write(*,*)'Nao eh uma matriz triangular superior'
      else if(VouF.EQ.1)then
       write(*,*)'Esta matriz eh uma matriz triangular superior'
      end if 
      write(*,*)            
 
      Write(*,300)'========Matriz_A======='
300   FORMAT(A50)
c escrevendo a matriz, fiz com looping da linha externo. Basta dar o formato de saida maximo, mais pratico!!! Porem, gasta mais espaços, pois sempre estrapola.      
      DO l=1,n
        write(*,30)(A(l,c),c=1,n)
30      Format(100F13.3)
      end do
      write(*,*)
      Write(*,*)'Aperte uma tecla para sair'        
      read(*,*)          
c     nao esquecer do End             
      end    