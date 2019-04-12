      subroutine HOGRE
     $(N,NSL,IQLYM,IRKS,RKI,IRLS,RLI)
C
C     Rudolf Loeser, 1989 Aug 21
C---- Deletes rates-related input in non-LYMAN runs.
C     !DASH
      save
C     !DASH
      real*8 RKI, RLI
      integer IQLYM, IRKS, IRLS, LUEO, N, NSL
      logical LRK, LRKS, LRL, LRLS
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external PALI, POLK, MESHED, MASHED, HI, BYE
C
C               IRKS(NSL), RKI(N,NSL), IRLS(NSL), RLI(N,NSL)
      dimension IRKS(*),   RKI(*),     IRLS(*),   RLI(*)
C
      call HI ('HOGRE')
C     !BEG
      if(IQLYM.le.0) then
        call PALI     (N, NSL, RKI, LRK)
        call POLK     (NSL, IRKS, LRKS)
        call PALI     (N, NSL, RLI, LRL)
        call POLK     (NSL, IRLS, LRLS)
C
        if((LRK.or.LRKS.or.LRL.or.LRLS)) then
          call MESHED ('HOGRE', 3)
  100     format(' ','When LYMAN = off, then ',A,' is not acceptable, ',
     $               'and has been rectified.')
          if(LRK) then
            write (LUEO,100) 'RK .ne. 0'
          end if
          if(LRKS) then
            write (LUEO,100) 'IRKCOMP .eq. 0'
          end if
          if(LRL) then
            write (LUEO,100) 'RL .ne. 0'
          end if
          if(LRLS) then
            write (LUEO,100) 'IRLCOMP .eq.0'
          end if
          call MASHED ('HOGRE')
        end if
      end if
C     !END
      call BYE ('HOGRE')
C
      return
      end
