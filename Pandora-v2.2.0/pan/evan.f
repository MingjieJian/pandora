      subroutine EVAN
     $(IPOP,H,LIMD,XLM,TE,POPN,BD,LEVEL,N,ORES,OREM)
C
C     Rudolf Loeser, 1978 Oct 06
C---- Computes a set of population ion bound-free opacity values.
C     !DASH
      save
C     !DASH
      real*8 BD, H, HNUKT, OREM, ORES, POPN, TE, TERM, TN, XLM, XMBARN,
     $       ZERO
      integer I, IPOP, J, LEVEL, LIMD, N
      logical DUMP
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (TUNI( 4),XMBARN)
C     !DASH
      external ZERO1, PROD, TAM, SANDBUR, BARBIE, CONMUL, MASHED,
     $         HI, BYE
C
C               H(LIMD), POPN(N,LIMD), BD(N,LIMD), OREM(N), ORES(N),
      dimension H(*),    POPN(N,*),    BD(N,*),    OREM(*), ORES(*),
C
C               TE(N)
     $          TE(*)
C     !EJECT
C
      call HI ('EVAN')
C     !BEG
      call ZERO1        (ORES, N)
      call ZERO1        (OREM, N)
C
      do 101 I = 1,N
        call SANDBUR    (I, IPOP, XLM, DUMP, 'EVAN')
C
        call PROD       (TE(I), XLM, 2, HNUKT, TN)
        do 100 J = 1,LIMD
          if(H(J).ne.ZERO) then
            call TAM    (TN, POPN(I,J), BD(I,J), H(J), TERM)
          else
            TERM = ZERO
          end if
C
          if(DUMP) then
            call BARBIE (J, TE(I), HNUKT, TN, POPN(I,J), BD(I,J), H(J),
     $                   TERM)
          end if
C
          if(J.eq.LEVEL) then
            ORES(I) = TERM
          else
            OREM(I) = OREM(I)+TERM
          end if
  100   continue
C
  101 continue
      if(DUMP) then
        call MASHED     ('EVAN')
      end if
C
      call CONMUL       (XMBARN, ORES, N)
      call CONMUL       (XMBARN, OREM, N)
C     !END
      call BYE ('EVAN')
C
      return
      end
