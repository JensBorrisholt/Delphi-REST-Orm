﻿using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using RestServer.Helpers;
using RestServer.Models;

namespace RestServer.Services
{
    public interface IUserService
    {
        Task<User> Authenticate(string username, string password);
    }

    public class UserService : IUserService
    {
        // users hardcoded for simplicity, store in a db with hashed passwords in production applications
        private readonly List<User> _users = new List<User>
        {
            new User { Id = 1, FirstName = "Test", LastName = "User", Username = "test", Password = "test" }
        };

        public async Task<User> Authenticate(string username, string password)
        {
            var user = await Task.Run(() => _users.SingleOrDefault(x => x.Username == username && x.Password == password));

            // return null if user not found
            return user?.WithoutPassword();

            // authentication successful so return user details without password
        }

    }
}
