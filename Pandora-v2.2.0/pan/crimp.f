      subroutine CRIMP
     $(Z,F,N,ZA,FA,M,VEC)
C
C     Rudolf Loeser, 2004 Jan 30
C---- Interpolates tables to augmented Z-scale, for KANTOR.
C     !DASH
      save
C     !DASH
      real*8 CRIT, F, FA, VEC, Z, ZA, ZERO
      integer I, LUEO, M, N, NGZ
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external PLUSD, DERE, MESHED, VECOUT, ABORT, HI, BYE
C
C               J = N+MXTAP
C
C               Z(N), F(N), ZA(J), FA(J), VEC(J)
      dimension Z(*), F(*), ZA(*), FA(*), VEC(*)
C
      data CRIT /7.D2/
C
      call HI ('CRIMP')
C     !BEG
      call PLUSD        (F, 1, N, NGZ)
      if(NGZ.eq.N) then
C
        do 100 I = 1,N
          VEC(I) = log(F(I))
  100   continue
        call DERE       (Z, 1, VEC, 1, N, ZA, 1, FA, 1, M, 1)
        do 102 I = 1,M
          if(FA(I).lt.CRIT) then
            FA(I) = exp(FA(I))
          else
            call MESHED ('CRIMP', 1)
            write (LUEO,101) I
  101       format(' ','Interpolated log is too large at I =',I6)
            call VECOUT (LUEO, FA, N, 'F and log(F)')
            call ABORT
          end if
  102   continue
C
      else
        call DERE       (Z, 1, F, 1, N, ZA, 1, FA, 1, M, 1)
      end if
C     !END
      call BYE ('CRIMP')
C
      return
      end
