      subroutine IMZAD
     $(TAURED,TNU,N,TNP,K,IS,IL,IB,TITLE,KODE,DUMP)
C
C     Rudolf Loeser, 1989 Dec 06
C---- Sets up and tests the reduced-TAU table TNP (length K),
C     for the QR-mapped method.
C     Also computes the selection indices IS and IL,
C     and the change-over index IB.
C---- Returns with KODE=1 if all seems OK, with KODE=0 otherwise.
C     !DASH
      save
C     !DASH
      real*8 TBAR, TLRGE, TNP, TNU, TSMLL
      integer IB, IL, IS, K, KODE, N
      logical DUMP, FIN, TAURED
      character TITLE*(*)
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
      equivalence (RZQ( 63),TSMLL)
      equivalence (RZQ( 64),TLRGE)
      equivalence (RZQ( 24),TBAR )
C     !DASH
      external GINGKO, FILLING, BENIN, MOVE1, NOTMORE, HI, BYE
C
C               TNU(N), TNP(K)
      dimension TNU(*), TNP(*)
C
      data FIN /.false./
C     !EJECT
C
      call HI ('IMZAD')
C     !BEG
      if(TAURED) then
C----   Get selection indices, and set up reduced set
        call GINGKO    (TNU, N, TLRGE, IL, TSMLL, IS, TNP, K)
C----   Get change-over index
        call NOTMORE   (TNP, K, TBAR, IB)
        if(DUMP) then
          call FILLING (TITLE, TNU, N, TNP, K, TSMLL, TLRGE, TBAR,
     $                  2, FIN, IS, IL, IB)
        end if
C----   Check whether TNP is OK
        call BENIN     (TNU, N, TNP, K, IS, IL, FIN, TLRGE, TITLE,
     $                  KODE)
C
      else
C
        K = N
        IS = 1
        IL = N+1
        KODE = 1
        call MOVE1     (TNU, N, TNP)
        call NOTMORE   (TNP, K, TBAR, IB)
      end if
C     !END
      call BYE ('IMZAD')
C
      return
      end
