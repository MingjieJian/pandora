      subroutine WROTH
     $(N,EM,F,HND)
C
C     Rudolf Loeser, 2006 Nov 28
C---- Adds magnetic term to partial HND, to get final new HND.
C     !DASH
      save
C     !DASH
      real*8 EM, F, HND, ONE, TWO
      integer I, K, KLIM, LUEO, N
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 3),TWO   )
C     !DASH
C     !EJECT
      external MESHED, VECOUT, ABORT, CONDIV, ARRSUB, HI, BYE
C
C               HND(N), EM(N)
      dimension HND(*), EM(*)
C
      data KLIM /10/
C
      call HI ('WROTH')
C     !BEG
      F = ONE
      K = 1
C
  100 continue
      do 102 I = 1,N
        if(EM(I).gt.HND(I)) then
          if(K.gt.KLIM) then
            call MESHED ('WROTH', 1)
            write (LUEO, 101) F
  101       format(' ','Magnetic pressure gives negative total ',
     $                 'hydrogen density.',10X,'F =',1PE20.12)
            call VECOUT (LUEO, HND, N, 'current partial HND')
            call VECOUT (LUEO, EM,  N, 'M x F'              )
            call ABORT
          else
            F = F/TWO
            K = K+1
            call CONDIV (TWO, EM, N)
            goto 100
          end if
        end if
  102 continue
C
      call ARRSUB       (HND, EM, HND, N)
C     !END
      call BYE ('WROTH')
C
      return
      end
