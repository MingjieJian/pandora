      subroutine MUTISM
     $(SA,CHKA,M,VEC,IPNT,KILROY,JJ,J,RCHK,DUMP)
C
C     Rudolf Loeser, 2004 Feb 10
C---- Delivers the next index to be treated (J), and
C     the associated value of RCHK, for SUMMIT.
C     !DASH
      save
C     !DASH
      real*8 CHKA, RCHK, SA, VEC
      integer I, IPNT, J, JJ, KODE, LUEO, M
      logical DUMP, KILROY
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external SMITE, SINGD, MESHED, MASHED, HI, BYE
C
C               J = N+MXTAP
C
C               SA(J), CHKA(J), VEC(J), IPNT(J)
      dimension SA(*), CHKA(*), VEC(*), IPNT(*)
C
      data KODE /0/
C     !EJECT
C
      call HI ('MUTISM')
C     !BEG
      if(KILROY) then
        KILROY = .false.
C
        do 100 I = 1,M
          call SMITE  (SA(I), CHKA(I), VEC(I))
  100   continue
        call SINGD    (VEC, M, KODE, IPNT)
C
        if(DUMP) then
          call MESHED ('MUTISM', 2)
          write (LUEO,101) KODE
  101     format(' ','Sorting rcheck: KODE =',I3,' and IPNT =')
          write (LUEO,102) (IPNT(I),I=1,M)
  102     format(' ',5I10,5X,5I10)
          call MASHED ('MUTISM')
        end if
C
      end if
C
      if(KODE.gt.0) then
        I = (M+1)-JJ
        J = IPNT(I)
        RCHK = VEC(I)
      else
        J = JJ
        call SMITE    (SA(J), CHKA(J), RCHK)
      end if
C     !END
      call BYE ('MUTISM')
C
      return
      end
