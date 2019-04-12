      subroutine PEBBLES
     $(AR,IPER,CNTRB,K,N,I,NOPAC,KIPE)
C
C     Rudolf Loeser, 1974 Mar 28
C---- Gets an edited set of ABS or EMIT percentages,
C     for contributors summaries.
C     (This is version 2 of PEBBLES.)
C     !DASH
      save
C     !DASH
      real*8 AR, CNTRB, SUM
      integer I, IPER, K, KIPE, N, NOPAC
C     !DASH
      external MOVE1, ARRSUM, FINN, HI, BYE
C
C               CNTRB(Nopac), AR(Nopac,N), IPER(Nopac,Numkon)
      dimension CNTRB(*),     AR(NOPAC,*), IPER(*)
C
      call HI ('PEBBLES')
C     !BEG
C---- Pick out K'th set
      call MOVE1  (AR(1,K), NOPAC, CNTRB)
C---- Find normalizing sum
      call ARRSUM (CNTRB, NOPAC, SUM)
C---- Form percentages
      call FINN   (IPER, CNTRB, SUM, I, NOPAC)
C     !END
      call BYE ('PEBBLES')
C
      return
      end
