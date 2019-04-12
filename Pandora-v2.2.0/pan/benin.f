      subroutine BENIN
     $(TNU,N,TNP,K,IS,IL,FIN,TLARGE,TITLE,KODE)
C
C     Rudolf Loeser, 1969 Sep 25
C---- Checks the TNP table.
C     If OK, returns with KODE=1,
C                but with KODE=0 otherwise.
C     !DASH
      save
C     !DASH
      real*8 FIVE, TLARGE, TNP, TNU
      integer IL, IS, K, KODE, MESS, N
      logical FIN
      character TITLE*(*)
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 6),FIVE  )
C     !DASH
      external TOGO, HI, BYE
C
C               TNU(N), TNP(N)
      dimension TNU(*), TNP(*)
C
      call HI ('BENIN')
C     !BEG
      KODE = 1
      MESS = 0
      if(K.lt.4) then
        KODE = 0
        if(TNU(3).le.TLARGE) then
          MESS = 1
        else
          MESS = 2
        end if
      else if((.not.FIN).and.(TNP(K).lt.FIVE)) then
        MESS = 3
      end if
C
      if(MESS.gt.0) then
        call TOGO (MESS, TNU, N, TNP, K, IS, IL, TITLE)
      end if
C     !END
      call BYE ('BENIN')
C
      return
      end
