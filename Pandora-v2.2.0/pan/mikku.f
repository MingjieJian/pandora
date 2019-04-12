      subroutine MIKKU
     $(LL,LIST,N,NL,AIN,AOUT,SIG,KODE)
C
C     Rudolf Loeser, 1990 Dec 03
C---- Makes a compressed version of a data array from the upper-level
C     charge exchange calculation, for plotting.
C
C     KODE = 1: log,  KODE = 0: lin.
C     !DASH
      save
C     !DASH
      real*8 AIN, AOUT, SIG, ZERO
      integer I, J, KODE, L, LIST, LL, N, NL
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external HI, BYE
C
C               LIST(LL), AIN(N,NL), AOUT(N,LL)
      dimension LIST(*),  AIN(N,*),  AOUT(N,*)
C
      call HI ('MIKKU')
C     !BEG
      do 101 L = 1,LL
        J = LIST(L)
        do 100 I = 1,N
          if(AIN(I,J).gt.ZERO) then
            if(KODE.eq.1) then
              AOUT(I,L) = log10(AIN(I,J))
            else
              AOUT(I,L) = AIN(I,J)
            end if
          else
            AOUT(I,L) = SIG
          end if
  100   continue
  101 continue
C     !END
      call BYE ('MIKKU')
C
      return
      end
