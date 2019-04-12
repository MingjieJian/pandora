      subroutine BRILL
     $(Z,TE,OPAC,WAVE,LTYPE,NW,ROSSK,ROSST,TEFF,IMG,W)
C
C     Rudolf Loeser, 1986 Mar 17
C---- Supervises the calculations of
C     Rosseland-mean opacity and optical depth, and
C     effective temperature.
C     !DASH
      save
C     !DASH
      real*8 OPAC, ROSSK, ROSST, TE, TEFF, W, WAVE, Z
      integer IFREQ, IMG, IN, IOP, IS, IWAF, LTYPE, MOX, N, NF, NW
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C     !DASH
      external KOKAKO, AUBURN, POGGE, PLOIMA, PRION, ZERO1, WGIVE,
     $         HI, BYE
C
      dimension W(*)
C
C               Z(N), IMG(N), OPAC(N,NW), WAVE(NW), LTYPE(NW), TEFF(N),
      dimension Z(*), IMG(*), OPAC(*),    WAVE(*),  LTYPE(*),  TEFF(*),
C
C               ROSST(N), ROSSK(N), TE(N)
     $          ROSST(*), ROSSK(*), TE(*)
C
      dimension IN(3)
      equivalence
     $(IN( 1),IFREQ ),(IN( 2),IWAF  ),(IN( 3),IOP   )
C     !EJECT
C
      call HI ('BRILL')
C     !BEG
C     (Get, and allocate, W allotment)
      call KOKAKO   (IN,IS,MOX,'BRILL')
C
C---- Get list of relevant wavelengths (length NF)
      call POGGE    (OPAC,WAVE,LTYPE,NW,N,NF,W(IWAF),W(IOP))
      if(NF.gt.0) then
C----   Compute Rosseland optical depth
        call AUBURN (NF,W(IWAF),W(IFREQ))
        call PRION  (NF,W(IWAF),W(IFREQ),W(IOP),N,Z,TE,ROSSK,ROSST,
     $               IMG,W)
C----   Compute effective temperature
        call PLOIMA (N,Z,TE,ROSSK,TEFF,W)
      else
        call ZERO1  (ROSST,N)
        call ZERO1  (ROSSK,N)
        call ZERO1  (TEFF ,N)
      end if
C
C     (Give back W allotment)
      call WGIVE    (W,'BRILL')
C     !END
      call BYE ('BRILL')
C
      return
      end
