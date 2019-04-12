      subroutine ALEPH
     $(X,W,XLM,OPAC,TAU,IMG)
C
C     Rudolf Loeser, 1971 Feb 03
C---- Computes Continuum Optical Depth.
C     !DASH
      save
C     !DASH
      real*8 OPAC, TAU, W, X, XLM
      integer IMG, KODE
      character LABEL*100
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external AMUCK, HALT, HI, BYE
C
      dimension X(*), W(*)
C
C               OPAC(N), TAU(N), IMG(N)
      dimension OPAC(*), TAU(*), IMG(*)
C
      call HI ('ALEPH')
C     !BEG
      write (LABEL,100) XLM
  100 format('Continuum Optical Depth, Lambda =',1PE20.12)
C
      call AMUCK  (X, W, OPAC, IMG, LABEL, TAU, KODE)
C
      if(KODE.gt.0) then
        write (MSSLIN(1),101) LABEL(:60)
  101   format(A60,': is unacceptable.')
        call HALT ('ALEPH', 1)
      end if
C     !END
      call BYE ('ALEPH')
C
      return
      end
