program tic_tac_toe
    implicit none
    character(len=1), dimension(3, 3) :: board
    character(len=1) :: player
    integer :: row, col, turn
    logical :: win

    ! Initialize the board and players
    board = ' '
    player = 'X'

    print *, "Welcome to Tic-Tac-Toe!"

    ! Game loop
    do turn = 1, 9
        ! Draw the board
        call draw_board(board)

        ! Prompt for valid input
        do
            print *, "Player ", player, ", enter row (0-2) and column (0-2): "
            read(*, *) row, col

            if (board(row + 1, col + 1) /= ' ' .or. row < 0 .or. row > 2 .or. col < 0 .or. col > 2) then
                print *, "Invalid move. Try again."
            else
                exit ! Valid input, exit the loop.
            end if
        end do

        ! Make the move
        board(row + 1, col + 1) = player

        ! Check for a win after making a move
        win = check_win(board, player)
        if (win) then
            call draw_board(board)
            print *, "Player ", player, " wins!"
            exit ! Exit the loop after a win.
        end if

        ! Switch to the other player
        if (player == 'X') then
            player = 'O'
        else
            player = 'X'
        end if
    end do

    ! End of the game
    call draw_board(board)

    ! Check for a draw
    if (turn == 9 .and. .not. check_win(board, 'X') .and. .not. check_win(board, 'O')) then
        print *, "It's a draw!"
    end if

contains

    ! Subroutine to draw the Tic-Tac-Toe board
    subroutine draw_board(board)
        character(len=1), dimension(3, 3), intent(in) :: board
        integer :: i

        print *, "-------------------------"
        do i = 1, 3
            write(*,'(A)', advance='no') "|      "
            write(*,'(A)', advance='no') board(i, 1), "      |      "
            write(*,'(A)', advance='no') board(i, 2), "      |      "
            write(*,'(A)', advance='no') board(i, 3), "      |"
            print *  ! Finish the current line
            print *, "-------------------------"
        end do
    end subroutine draw_board

    ! Function to check for a win
    logical function check_win(board, player)
        character(len=1), dimension(3, 3), intent(in) :: board
        character(len=1), intent(in) :: player
        integer :: i

        check_win = .false.

        ! Check rows and columns
        do i = 1, 3
            if (board(i, 1) == player .and. board(i, 2) == player .and. board(i, 3) == player) then
                check_win = .true.
                return
            end if
            if (board(1, i) == player .and. board(2, i) == player .and. board(3, i) == player) then
                check_win = .true.
                return
            end if
        end do

        ! Check diagonals
        if (board(1, 1) == player .and. board(2, 2) == player .and. board(3, 3) == player) then
            check_win = .true.
            return
        end if
        if (board(1, 3) == player .and. board(2, 2) == player .and. board(3, 1) == player) then
            check_win = .true.
            return
        end if
    end function check_win

end program tic_tac_toe
