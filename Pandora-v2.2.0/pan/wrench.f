      subroutine WRENCH
     $(ONAME,IQT,N,OSCAR,IPNT)
C
C     Rudolf Loeser, 1987 Jul 15
C---- Sorts the table of option names.
C     (This is version 2 of WRENCH.)
C     !DASH
      save
C     !DASH
      integer I, IPNT, IQT, LPNT, LUEO, N
      character ONAME*8, OSCAR*20
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external SINGC, MESHED, MASHED, HI, BYE
C
C               IPNT(N), IQT(N), ONAME(N), OSCAR(N)
      dimension IPNT(*), IQT(*), ONAME(*), OSCAR(*)
C
      call HI ('WRENCH')
C     !BEG
      do 101 I = 1,N
        write (OSCAR(I),100) ONAME(I),IQT(I),I
  100   format(A8,6X,I2,I4)
  101 continue
C
      call SINGC    (OSCAR, N, LPNT, IPNT)
C
      if(LPNT.le.0) then
        call MESHED ('WRENCH', 3)
        write (LUEO,102) LPNT
  102   format(' ','LPNT =',I12,': unable to sort OPTION names.')
        call MASHED ('WRENCH')
      end if
C     !END
      call BYE ('WRENCH')
C
      return
      end
