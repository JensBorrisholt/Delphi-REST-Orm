using Microsoft.AspNetCore.Mvc;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace RestServer.Controllers
{
    //[Route("[test]")]
    //[ApiController]
    public class TestController : ControllerBase
    {
        public IActionResult Test(string id)
        {
            return Ok("ID");
        }
    }
}
