      subroutine BINEL
     $(NO)
C
C     Rudolf Loeser, 1999 Jan 07
C---- Prints diffusion contral parameters, for PATCH.
C     !DASH
      save
C     !DASH
      real*8 FZION, ZXMIN
      integer I, IBTSW, IDFDM, IDFDS, IPDEE, KDFD1, LDFD1, NEFDF, NO
      character LAB*1
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (RZQ(112),ZXMIN)
      equivalence (RZQ(128),FZION)
      equivalence (KZQ(111),IDFDM)
      equivalence (KZQ(153),KDFD1)
      equivalence (KZQ(166),IBTSW)
      equivalence (KZQ(163),NEFDF)
      equivalence (KZQ(165),IPDEE)
      equivalence (KZQ(203),IDFDS)
      equivalence (KZQ(204),LDFD1)
C     !DASH
      external  NIBBLE, LINER, HI, BYE
C
      dimension LAB(4)
C
      call HI ('BINEL')
C     !BEG
      write (NO,100) ZXMIN,FZION
  100 format(' ','Criterion for ZION: ZXMIN =',1PE12.4/
     $       ' ','ZION multiplier:    FZION =',E12.4)
C     !EJECT
      call LINER  (1,NO)
      call NIBBLE (LAB,(IDFDM+1),1,2)
      write (NO,101) (LAB(I),I=1,2)
  101 format(' ','Choice of method for d-coefficients is controlled ',
     $           'by IDFDM:'/
     $       ' ','  IDFDM     =  0',A,', original'/
     $       ' ','            =  1',A,', improved')
      call NIBBLE (LAB,NEFDF,1,2)
      write (NO,102) (LAB(I),I=1,2)
  102 format(' ','Which NE to use for improved d-coefficients is ',
     $           'controlled by NEFDF:'/
     $       ' ','     NEFDF  =  1',A,', use internally-computed NE'/
     $       ' ','            =  2',A,', use regular Pandora NE')
      call NIBBLE (LAB,(IDFDS+1),1,2)
      write (NO,103) (LAB(I),I=1,2)
  103 format(' ','Smoothing of d-coefficients is controlled ',
     $           'by IDFDS:'/
     $       ' ','     IDFDS  =  0',A,', do not smooth'/
     $       ' ','            =  1',A,', do smoothing')
      call NIBBLE (LAB,(IPDEE+1),1,2)
      write (NO,104) (LAB(I),I=1,2)
  104 format(' ','Printing of the full d-coefficients array is ',
     $           'controlled by IPDEE:'/
     $       ' ','     IPDEE  =  0',A,', do not print (plot only)'/
     $       ' ','            =  1',A,', do print (and plot)')
C
      call LINER  (1,NO)
      call NIBBLE (LAB,(IBTSW+1),1,3)
      write (NO,105) (LAB(I),I=1,3)
  105 format(' ','Choice of method for beta (He-II) is controlled ',
     $           'by IBETSW:'/
     $       ' ','  IBETSW    =  0',A,', beta = average of HEK, SHE2'/
     $       ' ','            =  1',A,', beta = HEK'/
     $       ' ','            =  2',A,', beta = SHE2 (total Helium-II)')
C
      call LINER  (1,NO)
      call NIBBLE (LAB,KDFD1,1,4)
      write (NO,106) (LAB(I),I=1,4)
  106 format(' ','Choice of method for first derivatives is ',
     $           'controlled by KDIFD1:'/
     $       ' ','  KDIFD1    =  1',A,', "bridging" interval, ',
     $           'no smoothing'/
     $       ' ','            =  2',A,', "regular" smoothing, ',
     $           'then average slope'/
     $       ' ','            =  3',A,', cubic spline, then ',
     $           '"irregular" smoothing'/
     $       ' ','            =  4',A,', average slope (no smoothing)')
      call NIBBLE (LAB,(LDFD1+1),1,2)
      write (NO,107) (LAB(I),I=1,2)
  107 format(' ','Smoothing of derivatives is controlled by LDFD1:'/
     $       ' ','     LDFD1  =  0',A,', do not smooth'/
     $       ' ','            =  1',A,', do smoothing')
C     !END
      call BYE ('BINEL')
C
      return
      end
