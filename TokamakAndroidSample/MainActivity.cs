using Android.App;
using Android.Widget;
using Android.OS;
using Renci.SshNet;
using System.IO;
using Tokamak;
using static Tokamak.Core;

namespace TokamakAndroidSample
{
	[Activity(Label = "TokamakAndroidSample", MainLauncher = true, Icon = "@mipmap/icon")]
	public class MainActivity : Activity
	{
		int count = 1;

		protected override void OnCreate(Bundle savedInstanceState)
		{
			base.OnCreate(savedInstanceState);

			// Set our view from the "main" layout resource
			SetContentView(Resource.Layout.Main);

			// Get our button from the layout resource,
			// and attach an event to it
			Button button = FindViewById<Button>(Resource.Id.myButton);

			button.Click += delegate
			{
				
				var script1 = @"

			name = ""user_name""
			port = 22
			ip = ""127.0.0.1""


		";

				var comp = new Core.Compiler();

				var exp = comp.compile(script1);



				var myReactor = new Core.IntegerConfinementUnit(exp);

			};
		}

		public void Connect()
		{
			ConnectionInfo ConnNfo = new ConnectionInfo("hostOrIP", 22, "username",
				new AuthenticationMethod[]{

                new PasswordAuthenticationMethod("username","password"),


				}
			);


			//// Upload A File
			//using (var sftp = new SftpClient(ConnNfo))
			//{
			//	string uploadfn = "Renci.SshNet.dll";

			//	sftp.Connect();
			//	sftp.ChangeDirectory("/tmp/uploadtest");
			//	using (var uplfileStream = System.IO.File.OpenRead(uploadfn))
			//	{
			//	//	sftp.UploadFile(uplfileStream, uploadfn, true);
			//	}
			//	sftp.Disconnect();
			//}
		}
	}



}

