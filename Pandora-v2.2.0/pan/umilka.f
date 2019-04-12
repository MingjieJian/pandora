      subroutine UMILKA
     $(ZION,WT,N)
C
C     Rudolf Loeser, 1999 Jan 07
C---- Sets up B1-weights, for MULIAK.
C     !DASH
      save
C     !DASH
      real*8 WT, ZION
      integer KB1WA, KB1WB, KB1WS, N
      logical KAOK, KBOK, lummy
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
      equivalence (KZQ(167),KB1WA)
      equivalence (KZQ(168),KB1WB)
      equivalence (KZQ(179),KB1WS)
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external MOVE1, MOLLUSC, ZERO1, ONE1, HALT, HI, BYE
C
C               ZION(N), WT(N)
      dimension ZION(*), WT(*)
C     !EJECT
C
      call HI ('UMILKA')
C     !BEG
      if((KB1WS.ge.1).and.(KB1WS.le.4)) then
        goto (101, 102, 103, 104), KB1WS
  101   continue
          call ZERO1     (WT, N)
          goto 100
  102   continue
          call ONE1      (WT, N)
          goto 100
  103   continue
          call MOVE1     (ZION, N, WT)
          goto 100
  104   continue
          KAOK = (KB1WA.ge.1).and.(KB1WA.le.N)
          KBOK = (KB1WB.ge.1).and.(KB1WB.le.N)
          if(KAOK.and.KBOK) then
            call MOLLUSC (lummy, WT, N, KB1WA, KB1WB)
            goto 100
          end if
      end if
C
C---- Error
      write (MSSLIN(1),105) KB1WS, KB1WA, KB1WB
  105 format('Setting up b-weights: KB1WS =',I12,5X,'KB1WA =',I12,5X,
     $       'KB1WB =',I12)
      call HALT          ('UMILKA', 1)
C
  100 continue
C     !END
      call BYE ('UMILKA')
C
      return
      end
