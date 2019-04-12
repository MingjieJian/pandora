      subroutine MANID
     $(NO,IQHEA)
C
C     Rudolf Loeser, 1999 Jan 07
C---- Prints HEABD-related stuff, for PATCH.
C     !DASH
      save
C     !DASH
      real*8 CHEFL, HEABL, RFHEA
      integer IHEAB, IHEDF, IQHEA, NO
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
      equivalence (RZQ(100),CHEFL)
      equivalence (RZQ(115),HEABL)
      equivalence (KZQ( 83),IHEAB)
      equivalence (RZQ(116),RFHEA)
      equivalence (KZQ( 47),IHEDF)
C     !DASH
      external LINER, HI, BYE
C
      call HI ('MANID')
C     !BEG
      if(IHEDF.gt.0) then
        write (NO,100)
  100   format(' ','The He-I and He-II populations are set = 0 ',
     $             '(input parameter IHEDF = 1).')
      else
        write (NO,101)
  101   format(' ','Calculation of depth-dependent Helium abundance ',
     $             'coefficient depends on option HEABD.')
        call LINER (1,NO)
        if(IQHEA.gt.0) then
          write (NO,102) CHEFL,HEABL,IHEAB,RFHEA
  102     format(' ','Depth-dependent Helium abundance coefficient ',
     $               '(RHEAB) will be computed using the following ',
     $               'parameters:',1P/
     $           ' ','Helium flow constant:          CHEFLOW =',E12.4/
     $           ' ','Abundance coefficient limit:     HEABL =',E12.4/
     $           ' ','Helium abundance depth index:    IHEAB =',I5/
     $           ' ','Abund. coeff. reduction factor: RFHEAB =',E12.4)
        end if
      end if
C     !END
      call BYE ('MANID')
C
      return
      end
