      subroutine ERYNGO
     $(N,RR,XA,XJNU)
C
C     Rudolf Loeser, 1981 Jul 22
C---- Sets default Jnu, for CRANE.
C     !DASH
      save
C     !DASH
      real*8 ONE, RR, XA, XJNU
      integer I, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external DIVIDE, HI, BYE
C
C               RR(N), XA(N), XJNU(N)
      dimension RR(*), XA(*), XJNU(*)
C
C
      call HI ('ERYNGO')
C     !BEG
      do 100 I = 1,N
        call DIVIDE (RR(I),(ONE-XA(I)),XJNU(I))
  100 continue
C     !END
      call BYE ('ERYNGO')
C
      return
      end
