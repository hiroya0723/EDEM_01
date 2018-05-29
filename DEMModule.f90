module DEMModule
    use MethodModule
    implicit none
    
    real(8) :: delta_t = 0.01
contains
    function loop(points, springs, x, d) result(tu)
        real(8) points(:,:)
        real(8) springs(:,:,:)
        real(8) x(:,:)
        real(8) d(:,:)
        
        real(8), allocatable :: x_old(:,:)
        real(8), allocatable :: u(:,:)
        real(8), allocatable :: spring_flag(:,:)
        real(8), allocatable :: e_mat(:,:,:), f_mat(:,:,:)
        integer n, i, j
        real(8) tu 

        f_mat = 0.0d0
        
        call makeD(x,d)
        !call checkSpringBreak(spring_flag)
        !call calcU(x, x_old, u)
        call calcF_MAT(x, u, springs, e_mat, f_mat)
        tu = 0.0d0
    end function loop
    
    function initD(points) result(d0)
        real(8), allocatable :: d0(:,:)
        real(8) points(:,:)
        integer n, i, j
        
        n = size(points, 1)
        allocate(d0(n,n))
        do i = 1,n
            do j = 1,n
                if(i > j) then
                    d0(i,j) = points(i,4) + points(j,4)
                else
                    d0(i,j) = 0.0d0
                endif
            enddo
        enddo
    end function initD
    
    function initX(points) result(x0)
        real(8), allocatable :: x0(:,:)
        real(8) points(:,:)
        integer n, i
        
        n = size(points, 1)
        allocate(x0(n,2))
        do i = 1, n
            x0(i,1) = points(i,2)
            x0(i,2) = points(i,3)
        enddo
    end function initX
    
    subroutine makeD(x, d)
        real(8) x(:,:)
        real(8) d(:,:)
        integer n, i, j
        
        n = size(x,1)
        print *, n
        do i = 1, n
            do j = 1, n
                if(i > j) then
                    d(i,j) = sqrt((x(i,1) - x(j,1))**2 + (x(i,2) - x(j,2))**2)
                else
                    d(i,j) = 0.0d0
                endif
            enddo
        enddo
    end subroutine makeD
    
    subroutine checkSpringFlag(spring_flag)
        integer spring_flag(:,:)
        
        print *, "checkSpringFlag"
    end subroutine checkSpringFlag
    
    subroutine calcF_MAT(x, u, springs, e_mat, f_mat)
        real(8) x(:,:)
        real(8) u(:,:)
        real(8) springs(:,:,:)
        real(8) e_mat(:,:,:)
        real(8) f_mat(:,:,:)
        real(8) f(2)
        integer n, i, j
        
        n = size(f_mat,1)
        do i = 2, n
            do j = 1, (i-1)
                f = calcf(x(i,:), x(j,:), u(i,:), u(j,:), springs(i,j,:), e_mat(i,j,:))
                f_mat(i,j,:) = f
                f_mat(j,i,:) = -f
            enddo
        enddo
    end subroutine calcF_MAT
    
    function calcf(x_i, x_j, u_i, u_j, spring, e) result(f)
        integer i, j
        real(8) x_i(:), x_j(:), u_i(:), u_j(:), spring(:), e(:)
        real(8) d_un, d_us, alpha
        real(8) fn, fs, f(2)
        
        alpha = calcAlpha(x_i, x_j)
        d_un = (x_i(1) - x_j(1))*cos(alpha) + (x_i(2) - x_j(2))*sin(alpha)
        d_us = -(x_i(1) - x_j(1))*sin(alpha) + (x_i(2) - x_j(2))*cos(alpha)
        
        e(1) = e(1) + spring(1) * d_un
        e(2) = e(2) + spring(2) * d_us

        fn = e(1) + spring(3) * d_un / delta_t
        fs = e(2) + spring(4) * d_us / delta_t
        
        f(1) = -fn*cos(alpha) + fs*sin(alpha)
        f(2) = -fn*sin(alpha) - fs*sin(alpha)
    end function calcf
    
    function calcAlpha(x1, x2) result(alpha)
        real(8) x1(:), x2(:)
        real(8) alpha
        alpha = atan2((x1(1) - x2(1)), (x1(2) - x2(2)))
    end function calcAlpha
    
    function makeSpring(filename, n) result(springs)
        character(*) filename
        real(8), allocatable :: springs(:,:,:), tmp(:,:)
        integer n, m, i, j, k
        
        tmp = readFile('spring.dat', 10)
        allocate(springs(n,n,8))
        springs = 0.0d0
        m = size(tmp,1)
        do i = 1, m
            springs(int(tmp(i,1)),int(tmp(i,2)),:) = tmp(i,3:)
        enddo
    end function makeSpring
end module DEMModule