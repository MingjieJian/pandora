      subroutine RHEBAN
     $(N,A,B,C,D,E,XM,ALFA,BETA)
C
C     Rudolf Loeser, 2000 Nov 15
C---- Prints results of alpha & beta normalization.
C     !DASH
      save
C     !DASH
      real*8 A, ALFA, B, BETA, C, D, E, XM
      integer I, LUEO, N
      character BLANK*1, MARK*1, STAR*1
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
      equivalence (SYMBS(45),STAR  )
C     !DASH
      external LINER, SHIM, HI, BYE
C
C               A(N), B(N), C(N), D(N), XM(N), ALFA(N), BETA(N), E(N)
      dimension A(*), B(*), C(*), D(*), XM(*), ALFA(*), BETA(*), E(*)
C
      call HI ('RHEBAN')
C     !BEG
      call LINER  (2, LUEO)
      write (LUEO,100)
  100 format(' ','Normalization of alpha & beta'//
     $       ' ',8X,'a=alpha(old)',4X,'b=beta(old)',6X,'c=(1-a-b)',
     $           5X,'d=edited c',3X,'e=smoothed d',3X,'m=(1-e)/(a+b)',
     $           2X,'alpha(new)=ma',3X,'beta(new)=mb')
      call LINER  (1, LUEO)
C
      do 102 I = 1,N
        MARK = BLANK
        if(E(I).ne.C(I)) then
          MARK = STAR
        end if
        write (LUEO,101) I,A(I),B(I),C(I),D(I),E(I),MARK,XM(I),ALFA(I),
     $                   BETA(I)
  101   format(' ',I5,1P5E15.7,A1,3E15.7)
        call SHIM (I, 5, LUEO)
  102 continue
C     !END
      call BYE ('RHEBAN')
C
      return
      end
