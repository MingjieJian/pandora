      subroutine ACAMAS
     $(N,Z,R1N,MRR,FRR,R,AS,DRP,DR,AD,TIT)
C
C     Rudolf Loeser, 1981 Aug 30
C---- Dumps for continuous emission calculation.
C     !DASH
      save
C     !DASH
      real*8 AD, AS, DR, DRP, FRR, R, R1N, Z
      integer I, LUEO, MRR, N
      character TIT*(*)
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, HI, BYE
C
C               Z(N), FRR(MRR), R(N), AS(N), DRP(MRR), DR(MRR), AD(MRR)
      dimension Z(*), FRR(*),   R(*), AS(*), DRP(*),   DR(*),   AD(*)
C
      call HI ('ACAMAS')
C     !BEG
      call LINER   (3, LUEO)
      write (LUEO,100) TIT,R1N
  100 format(' ','Continuum ',A,' details',10X,'R1N=',1PE16.8//
     $       ' ',19X,'Z',15X,'R',14X,'AS')
      call LINER   (1, LUEO)
C
      write (LUEO,101) (I,Z(I),R(I),AS(I),I=1,N)
  101 format(5(' ',I4,1P3E16.8/))
C
      if(MRR.gt.0) then
        call LINER (2, LUEO)
        write (LUEO,102)
  102   format(' ',17X,'FRR',13X,'DRP',14X,'DR',14X,'AD')
        call LINER (1, LUEO)
C
        write (LUEO,103) (I,FRR(I),DRP(I),DR(I),AD(I),I=1,MRR)
  103   format(5(' ',I4,1P4E16.8/))
      end if
C     !END
      call BYE ('ACAMAS')
C
      return
      end
