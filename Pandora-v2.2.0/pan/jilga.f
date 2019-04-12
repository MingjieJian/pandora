      subroutine JILGA
     $(WK,K,WN,N,TNU,IS,IL,TITLE,DUMP)
C
C     Rudolf Loeser, 1989 Nov 02
C---- "Expands" a "reduced" WN matrix.
C     !DASH
      save
C     !DASH
      real*8 TNU, WK, WN
      integer IL, IS, K, LUEO, N
      logical DUMP
      character TITLE*(*)
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external SUMAC, ALDER, ASH, IOTA, MOVE1, LINER, HI, BYE
C
C               WK(K,K), WN(N,N), TNU(N)
      dimension WK(K,*), WN(N,*), TNU(*)
C
      call HI ('JILGA')
C     !BEG
      if(K.lt.N) then
C----   Expand the matrix
        call SUMAC   (WK, K, N, IS, IL, WN)
        if(IS.gt.1) then
C----     Set up elements corresponding to small TNU values
          call ALDER (WN, N, IS)
        end if
        if(IL.le.N) then
C----     Set up elements corresponding to large TNU values
          call ASH   (WN, N, TNU, IL)
        end if
        if(DUMP) then
          call IOTA  (WN, N, TITLE, 'WN as expanded')
        end if
C
      else
C----   Just copy as is
        call MOVE1   (WK, (N**2), WN)
        if(DUMP) then
          call LINER (2, LUEO)
          write (LUEO,100) K,N
  100     format(' ','K =',I4,'  and N =',I4,': no expansion needed.')
        end if
      end if
C     !END
      call BYE ('JILGA')
C
      return
      end
