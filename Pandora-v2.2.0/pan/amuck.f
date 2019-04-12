      subroutine AMUCK
     $(X,W,OPAC,IMG,LABEL,TAU,KODE)
C
C     Rudolf Loeser, 1980 Sep 22
C---- Computes Continuum Optical depth at wavelength XLM.
C     Returns with KODE .eq. 0 if all is ok, .gt. 0 if not.
C     (This is version 3 of AMUCK.)
C     !DASH
      save
C     !DASH
      real*8 OPAC, TAU, W, X
      integer IMG, KODE, LUEO, N
      logical lummy1, lummy2
      character LABEL*100
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external DISMAL, MESHED, PRIVET, MASHED, HI, BYE
C
      dimension X(*), W(*)
C
C               OPAC(N), TAU(N), IMG(N)
      dimension OPAC(*), TAU(*), IMG(*)
C
      call HI ('AMUCK')
C     !BEG
      call DISMAL   (X, W, 1, N, OPAC, TAU, LABEL, KODE,
     $               lummy1, lummy2, IMG)
C
      if(KODE.gt.0) then
        call MESHED ('AMUCK', 3)
        write (LUEO,100) LABEL,KODE
  100   format(' ',A//
     $         ' ','TAU is not monotonic at',I3,'. point.')
        call PRIVET (LUEO, TAU, N)
        call MASHED ('AMUCK')
      end if
C     !END
      call BYE ('AMUCK')
C
      return
      end
