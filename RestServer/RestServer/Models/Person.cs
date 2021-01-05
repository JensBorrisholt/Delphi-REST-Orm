using System;
using System.Collections.Generic;
using Microsoft.EntityFrameworkCore;

#nullable disable

namespace RestServer.Models
{
    public partial class Person
    {
        public string Id { get; set; }
        public int? Index { get; set; }
        public string Guid { get; set; }
        public string IsActive { get; set; }
        public string Balance { get; set; }
        public string Picture { get; set; }
        public int? Age { get; set; }
        public string EyeColor { get; set; }
        public string Name { get; set; }
        public string Gender { get; set; }
        public string Company { get; set; }
        public string Email { get; set; }
        public string Phone { get; set; }
        public string Address { get; set; }
        public string About { get; set; }
        public string Registered { get; set; }
        public double? Latitude { get; set; }
        public double? Longitude { get; set; }
        public string Greeting { get; set; }
        public string FavoriteFruit { get; set; }

        public Person UpdateValues(Person from)
        {
            if (Id != from.Id)
                throw new ApplicationException();

            foreach (var property in from.GetType().GetProperties())
                property.SetValue(this, property.GetValue(from));

            return this;
        }
    }
}
