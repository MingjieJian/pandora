      subroutine JOCOTE
     $(TAURED,TNU,N,FIN,TNP,K,IS,IL,TITLE,KODE,DUMP)
C
C     Rudolf Loeser, 1989 Dec 08
C---- Sets up and tests the reduced-TAU table TNP (length K),
C     for the RT weight matrix method.
C     Also computes the selection indices IS and IL.
C---- Returns with KODE=1 if all seems OK,
C             with KODE=0 otherwise.
C     !DASH
      save
C     !DASH
      real*8 TNP, TNU, TSMLL, dummy
      integer IL, IS, K, KODE, N, jummy
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
C     !DASH
      external BILOBA, FILLING, BENIN, MOVE1, HI, BYE
C
C               TNU(N), TNP(N)
      dimension TNU(*), TNP(*)
C
      call HI ('JOCOTE')
C     !BEG
      if(TAURED) then
C----   Get selection indices, and set up reduced set
        call BILOBA    (TNU, N, FIN, IL, TSMLL, IS, TNP, K)
        if(DUMP) then
          call FILLING (TITLE, TNU, N, TNP, K, TSMLL, dummy, dummy,
     $                  3, FIN, IS, IL, jummy)
        end if
C----   Check whether TNP is OK
        call BENIN     (TNU, N, TNP, K, IS, IL, FIN, TNU(N), TITLE,
     $                  KODE)
C
      else
        call MOVE1     (TNU, N, TNP)
        KODE = 1
        IS = 1
        IL = N+1
        K = N
      end if
C     !END
      call BYE ('JOCOTE')
C
      return
      end
