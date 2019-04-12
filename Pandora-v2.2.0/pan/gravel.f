      subroutine GRAVEL
     $(N,XLMT,TAUK,EP1,CQI,CQTAIL,MQT,KODE)
C
C     Rudolf Loeser, 1976 Dec 02
C---- Determines whether any EP1 values are negative, sets KODE=1
C     if yes, =0 if no, and sets up the QI array to deal with any
C     negative values.
C     !DASH
      save
C     !DASH
      real*8 CQI, CQTAIL, EP1, ONE, TAUK, XLMT, ZERO
      integer I, KODE, MQT, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external ROCK, HI, BYE
C
C               TAUK(N), EP1(N), CQI(N), CQTAIL(MQT)
      dimension TAUK(*), EP1(*), CQI(*), CQTAIL(*)
C
      call HI ('GRAVEL')
C     !BEG
      KODE = 0
C
      do 100 I = 1,N
        if((TAUK(I).gt.XLMT).and.(EP1(I).lt.ZERO)) then
          CQI(I) = ONE
          KODE =1
        end if
  100 continue
C
      if((KODE.gt.0).and.(MQT.gt.0)) then
        call ROCK (N,CQI,MQT,CQTAIL)
      end if
C     !END
      call BYE ('GRAVEL')
C
      return
      end
