      subroutine SPRUCE
     $(TAU,N,WN,REF,FIN,TMS,T,DT,SE2,SE3,SE4,SES2,SES3,SES4)
C
C     Rudolf Loeser, 1971 Jul 09 (revised 2000 Jan 25)
C---- Computes the RT weight matrix.
C     TMS controls the use of central parabolic segments.
C     !DASH
      save
C     !DASH
      real*8 DT, SE2, SE3, SE4, SES2, SES3, SES4, T, TAU, TMS, WN
      integer I, N
      logical FIN, REF, SMLLTAU
C     !DASH
      external ZERO1, ELM, OLMO, HEMLOCK, HI, BYE
C
C               TAU(N), WN(N,N), SE2(2*N), SE3(2*N), T(2*N), SES3(2*N),
      dimension TAU(*), WN(N,*), SE2(*),   SE3(*),   T(*),   SES3(*),
C
C               DT(2*N), SES2(2*N), SES4(2*N), SE4(2*N)
     $          DT(*),   SES2(*),   SES4(*),   SE4(*)
C
      call HI ('SPRUCE')
C     !BEG
C---- Initialize matrix
      call ZERO1     (WN,N*N)
C---- Initialize extended T table
      call ELM       (TAU,T,N)
C
C---- Loop over all rows
      do 100 I = 1,N
        SMLLTAU = T(I).le.TMS
C----   Set up DT (distances from the diagonal element)
        call OLMO    (T,T(I),DT,SE2,SES2,SE3,SES3,SE4,SES4,N)
C----   Compute current row
        call HEMLOCK (WN,N,I,FIN,REF,DT,SE2,SE3,SE4,SES2,SES3,SES4,
     $                SMLLTAU)
  100 continue
C     !END
      call BYE ('SPRUCE')
C
      return
      end
