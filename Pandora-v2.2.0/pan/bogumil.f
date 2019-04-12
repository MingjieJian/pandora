      subroutine BOGUMIL
     $(WTAB,NW,MF,ML)
C
C     Rudolf Loeser, 1994 Nov 02
C---- Sets up wavelength limits for ORIGINS and/or CONTRIBUTORS.
C     !DASH
      save
C     !DASH
      real*8 CORMN, CORMX, WTAB, ZERO
      integer MF, ML, NW
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
      equivalence (RZQ(145),CORMN)
      equivalence (RZQ(146),CORMX)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
C---- MOSTAR      as of 2000 Sep 26
      real*8      COREWL,COREWN
      integer     ICORE
      character   WLAB1*10,WLAB2*2,WLAB3*12,WLAB4*10,WLAB5*2
      logical     WAVENO,SLINE,WHOLE,RED,BLUE
      common      /MOSTAR1/ COREWL,COREWN
      common      /MOSTAR2/ ICORE
      common      /MOSTAR3/ WLAB1,WLAB2,WLAB3,WLAB4,WLAB5
      common      /MOSTAR4/ WAVENO,SLINE,WHOLE,RED,BLUE
C     Wavelength/Wavenumber print/plot controls (subroutine WUMBLE).
C     .
C     !DASH
C     !EJECT
      external NOTMORE, NOTLESS, HI, BYE
C
C               WTAB(KM or Nmkuse)
      dimension WTAB(*)
C
      call HI ('BOGUMIL')
C     !BEG
      if(WAVENO) then
C
        ML = NW
        if(CORMN.ge.ZERO) then
          call NOTLESS (WTAB, NW, CORMN, ML)
          if((ML.le.0).or.(ML.gt.NW)) then
            ML = NW
          end if
        end if
C
        MF = 1
        if(CORMX.ge.ZERO) then
          call NOTMORE (WTAB, NW, CORMX, MF)
          if((MF.le.0).or.(MF.gt.NW)) then
            MF = 1
          end if
        end if
C
      else
C
        MF = 1
        if(CORMN.ge.ZERO) then
          call NOTLESS (WTAB, NW, CORMN, MF)
          if((MF.le.0).or.(MF.gt.NW)) then
            MF = 1
          end if
        end if
C
        ML = NW
        if(CORMX.ge.ZERO) then
          call NOTMORE (WTAB, NW, CORMX, ML)
          if((ML.le.0).or.(ML.gt.NW)) then
            ML = NW
          end if
        end if
C
      end if
C     !END
      call BYE ('BOGUMIL')
C
      return
      end
