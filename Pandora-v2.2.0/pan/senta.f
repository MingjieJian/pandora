      subroutine SENTA
     $(A,TAU,N,LABEL,AOLD,DUMP)
C
C     Rudolf Loeser, 1979 Dec 02
C---- Edits negatives out of a function of optical depth, using a
C     logarithmic interpolation procedure.
C     (This is version 2 of SENTA.)
C     !DASH
      save
C     !DASH
      real*8 A, AOLD, TAU
      integer IG, IL, KODE, LGT, N
      logical DUMP
      character LABEL*(*)
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external PLUSD, MOVE1, LIMES, VALLID, LIONEL, HALT, RAPAR,
     $         HI, BYE
C
C               A(N), TAU(N), AOLD(N)
      dimension A(*), TAU(*), AOLD(*)
C     !EJECT
C
      call HI ('SENTA')
C     !BEG
C---- Count number of values .gt. 0
      call PLUSD       (A, 1, N, LGT)
C
      if(LGT.lt.N) then
C----   Editing needed
C
C----   Save for possible dump
        call MOVE1     (A, N, AOLD)
C----   Initialize limiting index
        IG = 0
  100   continue
C
C----     Find limiting indices of bad interval
          call LIMES   (A, N, IL, IG)
C----     Decide on processing options
          call VALLID  (IL, IG, N, LABEL, KODE)
          if(KODE.eq.2) then
C----       Edit
            call RAPAR (IL, IG, A, TAU)
            goto 100
          else if(KODE.eq.3) then
            goto 100
          else if(KODE.ne.1) then
            write (MSSLIN(1),101) KODE
  101       format('KODE =',I12,', which is not 1, 2, or 3.')
            call HALT  ('SENTA', 1)
          end if
C
        if(DUMP) then
          call LIONEL  (A, TAU, N, LABEL, AOLD, 'SENTA')
        end if
C
      end if
C     !END
      call BYE ('SENTA')
C
      return
      end
