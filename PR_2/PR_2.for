      implicit NONE
      real*8 id,M_id,T_pesquisa,Pesquisa,porcent
      integer n,s,co,cc,i
c 1 homem,2 mulher
c 3,4,5,6 cor dos olhos
c 7,8,9 cor do cabelo      
      n=-1
      While(n.lt.0)DO
       write(*,*)'Entre com a quantidade de pesquisados'
       Read(*,*)n
      EndWhile
      
      
      M_id=0.d0
      T_pesquisa=0.d0
      Pesquisa=0.d0 
      Do i=1,n
       s=-1
       while(s.lt.0.or.s.gt.2)do
        Write(*,*)'Entre com o sexo do ',i,' individuo'
        read(*,*)s
       EndWhile
       
       
       co=-1
       while(co.lt.2.or.co.gt.6)do
        write(*,*)'Entre com a cor dos olhos do ',i,' individuo'
        read(*,*)co
       End While
       
       cc=-1
       while(cc.lt.7.or.cc.gt.9)do
        Write(*,*)'Entre com a cor do cabelo do ',i,' individuo'
        read(*,*)cc
       EndWhile
       
       id=-2.d0
       while(id.Lt.-1.d0)Do
        Write(*,*)'Entre com a idade do ',i,' individuo'
        read(*,*)id
       endWhile
       
       if(id.GT.M_id)then
        M_id=id
       end if
       
       t_pesquisa=t_pesquisa+1.d0
       if(s.EQ.1)then
        pesquisa=pesquisa
        if(id.GT.37.d0.AND.id.LT.59)then
         pesquisa=pesquisa
         if(co.EQ.5)then
          pesquisa=pesquisa
          if(cc.EQ.8)then
           Pesquisa=Pesquisa+1.d0
          else
           pesquisa=pesquisa
          endif
         endif
        endif
       endif   
      EndDo 
      
      porcent=(pesquisa/T_pesquisa)*100.d0
      
      Write(*,*)'O cidadao mais velho tem:'
      Write(*,*)M_id
      Write(*,*)
      Write(*,*)'A porcentagem da pesquisa eh:'
      write(*,*)porcent
      write(*,*)
      write(*,*)'Aperte uma tecla para fechar'
      read(*,*)
      End