      subroutine RALLY
     $(ITAU,XLM,WLIN,TE,H1,H2,PFAC,EX,XNUM,XDEN,OPAC,SHL)
C
C     Rudolf Loeser, 2002 Sep 24
C---- Prints details of Lyman alpha background source function
C     calculation.
C     !DASH
      save
C     !DASH
      real*8 EX, H1, H2, OPAC, PFAC, SHL, TE, WLIN, XDEN, XLM, XNUM
      integer ITAU, LUEO
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, DASHER, HI, BYE
C
      call HI ('RALLY')
C     !BEG
      call LINER    (2, LUEO)
      call DASHER   (LUEO)
      call LINER    (1, LUEO)
C
      write (LUEO,100) XLM,ITAU,WLIN,TE,H1,H2,XNUM,PFAC,EX,XDEN,OPAC,SHL
  100 format(' ','Lyman-alpha background source function ',
     $           'calculation at LM =',1PE19.12,' and i =',I5//
     $       ' ','WLIN =',E19.12,5X,'TE =',E15.8,5X,'H1 =',E15.8,5X,
     $           'H2 =',E15.8/
     $       ' ','num =',E16.8,5X,'PFAC =',E16.8,5X,'exp =',E16.8,5X,
     $           'den =',E16.8/
     $       ' ','OPAC =',E15.8,5X,'SHL =',E15.8)
C     !END
      call BYE ('RALLY')
C
      return
      end
