      subroutine NABUSSI
     $(N,HEABD,HEMSS,IHEAB,CHEFL,HEABL,RFHEAB,CVM,IQVLG,ASTAR,ELL,CUE,
     $ CAY,WHY,EFF,RHEAB,DRHEAB,Z,RHEABO,DEE,HND,ZION,ZI,TE,ZT,Z2,Z3)
C
C     Rudolf Loeser, 1991 Jan 04
C---- Prints for THALIA.
C     !DASH
      save
C     !DASH
      real*8 ASTAR, CAY, CHEFL, CUE, CVM, DEE, DRHEAB, EFF, ELL, FZION,
     $       HEABD, HEABL, HEMSS, HND, RFHEAB, RHEAB, RHEABO, TE, WHY,
     $       Z, Z2, Z3, ZI, ZION, ZT, ZXMIN
      integer I, IHEAB, IQVLG, J, LUEO, N
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
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external ABJECT, LINER, HI, BYE
C
C               DEE(4,5,N), ELL(N), CUE(N), CAY(N), Z(N), Z2(N), Z3(N),
      dimension DEE(4,5,*), ELL(*), CUE(*), CAY(*), Z(*), Z2(*), Z3(*),
C
C               EFF(N), RHEAB(N), DRHEAB(N), RHEABO(N), WHY(N), HND(N),
     $          EFF(*), RHEAB(*), DRHEAB(*), RHEABO(*), WHY(*), HND(*),
C
C               ZION(N), ZI(N), TE(N), ZT(N)
     $          ZION(*), ZI(*), TE(*), ZT(*)
C     !EJECT
C
      call HI ('NABUSSI')
C     !BEG
      call ABJECT (LUEO)
      write (LUEO,100) IHEAB,HEABD,HEMSS
  100 format(' ','Calculation of depth-dependence of ',
     $           'Helium abundance.'//
     $       ' ','RHEAB  = Helium abundance relative to the ',
     $           'value at depth index IHEAB =',I4/
     $       ' ','DRHEAB = locally computed change of "RHEAB" at a ',
     $           'given depth relative to the subsequent depth,'/
     $       ' ','assuming no change of the interaction ',
     $           'coefficients d(i,j), and CHEFLOW = CVM = 0'//
     $       ' ','Helium abundance at depth # IHEAB =',1PE12.5/
     $       ' ','Helium mass =',E12.5)
      write (LUEO,101) CHEFL
  101 format(' ','Helium flow constant, CHEFLOW =',1PE13.5/
     $       ' ','  The constant CHEFLOW represents  NHE*(VHE + VM),',
     $           ' where  NHE  is the total Helium number density,'/
     $       ' ','  VHE  is the total Helium diffusion velocity, ',
     $           'and  VM  is the mass-motion velocity (velocities ',
     $           'measured inward in cm/s).'/
     $       ' ','  The value of  NHE*(-VP + VM)  at a ',
     $           'representative depth, where  VP  is the proton ',
     $           'diffusion velocity,'/
     $       ' ','  may be a reasonable value for CHEFLOW.')
      write (LUEO,102) HEABL,RFHEAB
  102 format(' ','Helium abundance limit factor, HEABL =',1PE12.5/
     $       ' ','  The final values of RHEAB are limited to the ',
     $           'range between  1/HEABL  and  HEABL .'/
     $       ' ','Helium abundance coefficient reduction factor, ',
     $           'RFHEAB =',E12.5/
     $       ' ','  RFHEAB multiplies d(2,1), d(2,3), d(2,4), ',
     $           'and d(2,5).')
      if(IQVLG.gt.0) then
        write (LUEO,103) CVM,ASTAR
  103   format(' ','Mass-motion velocity (km/s) at NH = 10**10, ',
     $             'CVM =',1PE12.5/
     $         ' ','Computed parameter a* =',E12.5)
      end if
C     !EJECT
      call LINER (2, LUEO)
      write (LUEO,104)
  104 format(' ',20X,'L',15X,'Q',15X,'K',15X,'y',15X,'F',11X,'RHEAB',
     $           10X,'DRHEAB')
      call LINER (1, LUEO)
      write (LUEO,105) (I,ELL(I),CUE(I),CAY(I),WHY(I),EFF(I),RHEAB(I),
     $                  DRHEAB(I),I=1,N)
  105 format(5(' ',I5,1P7E16.5/))
C
      call LINER (2, LUEO)
      write (LUEO,106) (I,I=1,5)
  106 format(' ',20X,'Z',7X,'old RHEAB',5(10X,'d(2,',I1,')'))
      call LINER (1, LUEO)
      write (LUEO,105) (I,Z(I),RHEABO(I),(DEE(2,J,I),J=1,5),I=1,N)
C
      call LINER (2, LUEO)
      write (LUEO,107)
  107 format(' ',18X,'HND',12X,'ZION',14X,'ZI',14X,'TE',14X,'ZT',
     $           14X,'Z2',14X,'Z3')
      call LINER (1, LUEO)
      write (LUEO,105) (I,HND(I),ZION(I),ZI(I),TE(I),ZT(I),Z2(I),Z3(I),
     $                I=1,N)
C
      call LINER (1, LUEO)
      write (LUEO,108) ZXMIN,FZION
  108 format(' ',16X,'ZXMIN',1PE16.5/
     $       ' ',16X,'FZION',  E16.5)
C     !END
      call BYE ('NABUSSI')
C
      return
      end
