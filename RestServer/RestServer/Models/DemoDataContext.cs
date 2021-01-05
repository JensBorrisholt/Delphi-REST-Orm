using System;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata;

#nullable disable

namespace RestServer.Models
{
    public partial class DemoDataContext : DbContext
    {
        public DemoDataContext()
        {
        }

        public DemoDataContext(DbContextOptions<DemoDataContext> options)
            : base(options)
        {
        }

        public virtual DbSet<Person> Persons { get; set; }

        protected override void OnConfiguring(DbContextOptionsBuilder optionsBuilder)
        {
            if (!optionsBuilder.IsConfigured)
            {
#warning To protect potentially sensitive information in your connection string, you should move it out of source code. You can avoid scaffolding the connection string by using the Name= syntax to read it from configuration - see https://go.microsoft.com/fwlink/?linkid=2131148. For more guidance on storing connection strings, see http://go.microsoft.com/fwlink/?LinkId=723263.
                optionsBuilder.UseSqlServer("Server=Dellserver;Database=DemoData;Integrated Security=True;");
            }
        }

        protected override void OnModelCreating(ModelBuilder modelBuilder)
        {
            modelBuilder.HasAnnotation("Relational:Collation", "Danish_Norwegian_CI_AS");

            modelBuilder.Entity<Person>(entity =>
            {
                entity.Property(e => e.Id)
                    .HasMaxLength(24)
                    .HasColumnName("id");

                entity.Property(e => e.About).HasColumnName("about");

                entity.Property(e => e.Address)
                    .HasMaxLength(43)
                    .HasColumnName("address");

                entity.Property(e => e.Age).HasColumnName("age");

                entity.Property(e => e.Balance)
                    .HasMaxLength(9)
                    .HasColumnName("balance");

                entity.Property(e => e.Company)
                    .HasMaxLength(7)
                    .HasColumnName("company");

                entity.Property(e => e.Email)
                    .HasMaxLength(22)
                    .HasColumnName("email");

                entity.Property(e => e.EyeColor)
                    .HasMaxLength(5)
                    .HasColumnName("eyeColor");

                entity.Property(e => e.FavoriteFruit)
                    .HasMaxLength(6)
                    .HasColumnName("favoriteFruit");

                entity.Property(e => e.Gender)
                    .HasMaxLength(10)
                    .HasColumnName("gender");

                entity.Property(e => e.Greeting)
                    .HasMaxLength(47)
                    .HasColumnName("greeting");

                entity.Property(e => e.Guid)
                    .HasMaxLength(36)
                    .HasColumnName("guid");

                entity.Property(e => e.Index).HasColumnName("index");

                entity.Property(e => e.IsActive)
                    .HasMaxLength(4)
                    .HasColumnName("isActive");

                entity.Property(e => e.Latitude).HasColumnName("latitude");

                entity.Property(e => e.Longitude).HasColumnName("longitude");

                entity.Property(e => e.Name)
                    .HasMaxLength(21)
                    .HasColumnName("name");

                entity.Property(e => e.Phone)
                    .HasMaxLength(17)
                    .HasColumnName("phone");

                entity.Property(e => e.Picture)
                    .HasMaxLength(25)
                    .HasColumnName("picture");

                entity.Property(e => e.Registered)
                    .HasMaxLength(26)
                    .HasColumnName("registered");
            });

            OnModelCreatingPartial(modelBuilder);
        }

        partial void OnModelCreatingPartial(ModelBuilder modelBuilder);
    }
}
