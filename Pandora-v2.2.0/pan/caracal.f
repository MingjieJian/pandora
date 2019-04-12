      subroutine CARACAL
     $(Z,TRA,KTRAS)
C
C     Rudolf Loeser, 1986 Oct 02
C---- Final editing and printing of "Artificial TAU".
C     !DASH
      save
C     !DASH
      real*8 TRA, Z
      integer J, KTRAS, LUEO, N, NT
      logical lummy
      character LINE*127
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 5),NT )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external IMPROVE, LINER, LABFIL, VECOUT, MESHED, MASHED, HI, BYE
C
C               Z(N), TRA(N)
      dimension Z(*), TRA(*)
C
      call HI ('CARACAL')
C     !BEG
      call IMPROVE    (TRA, Z, N, 'CARACAL', J, lummy)
C
      if(J.gt.0) then
C
        if(NT.gt.1) then
          call MESHED ('CARACAL', 3)
          call LABFIL ('Artificial TAU for RHO/W', LINE)
          call VECOUT (LUEO, TRA, N, LINE)
          call LINER  (2, LUEO)
          write (LUEO,100)
  100     format(' ','Artificial TAU is bad; individual Line Center ',
     $               'TAUs will be used.')
          call MASHED ('CARACAL')
        end if
C
        KTRAS = 0
      end if
C     !END
      call BYE ('CARACAL')
C
      return
      end
