using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.Json;
using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.Mvc.Infrastructure;
using Newtonsoft.Json;
using RestServer.Models;

// For more information on enabling Web API for empty projects, visit https://go.microsoft.com/fwlink/?LinkID=397860

namespace RestServer.Controllers
{
    [Route("[controller]")]
    [ApiController]
    public class ContactController : ControllerBase
    {
        // ReSharper disable once InconsistentNaming
        private static readonly Random _random = new Random();
        private static string RandomString(int length)
        {
            const string chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";
            return new string(Enumerable.Repeat(chars, length)
                .Select(s => s[_random.Next(s.Length)]).ToArray());
        }

        // GET: <ContactController>
        [Authorize, RequireHttps, HttpGet]
        public IEnumerable<Person> Get() => new DemoDataContext().Persons.ToList();


        // GET <ContactController>/5
        [HttpGet("{id}")]
        public Person Get(int id)
        {
            var result = new DemoDataContext().Persons.SingleOrDefault(e => e.Index == id);
            if (result == null)
                throw new ArgumentException($"No person by that id {id} found in the database");
            return result;
        }



        // POST <ContactController>
        [HttpPost]
        public Person Post([FromBody] JsonElement value)
        {
            var context = new DemoDataContext();
            var person = JsonConvert.DeserializeObject<Person>(value.ToString());
            person.Id = RandomString(24);
            person.Index = (context.Persons.Max(e => e.Index) ?? 0) + 1;
            context.Add(person);
            context.SaveChanges();
            return person;
        }

        // PUT <ContactController>/5
        [HttpPut("{id}")]
        public Person Put(int id, [FromBody] Person value)
        {
            var context = new DemoDataContext();
            var dbPerson = context.Persons.SingleOrDefault(e => e.Index == id);

            if (dbPerson == default(Person))
                throw new AmbiguousActionException($"No person by that id {id} found in the database");

            dbPerson.UpdateValues(value);
            context.Update(dbPerson);
            context.SaveChanges();
            return dbPerson;
        }

        // DELETE <ContactController>/5
        [HttpDelete("{id}")]
        public void Delete(int id)
        {
            var context = new DemoDataContext();
            var dbPerson = context.Persons.SingleOrDefault(e => e.Index == id);

            if (dbPerson == default(Person))
                throw new AmbiguousActionException($"No person by that id {id} found in the database");

            context.Persons.Remove(dbPerson);
            context.SaveChanges();
        }
    }
}
