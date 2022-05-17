module FaqNurses.Update exposing (init, update)

import FaqNurses.Types exposing (Faq, FormattedText(..), Model, Msg)
import Html.Attributes exposing (id)
import Return exposing (Return, singleton)


init : Return Msg Model
init =
    singleton
        { title = "FAQ for Internationally Educated Nurses - Flint"
        , heroTitle = "FAQ for Internationally Educated Nurses"
        , faqs = faqs
        }


update : Msg -> Model -> Return Msg Model
update _ model =
    singleton model


faqs : List Faq
faqs =
    [ { question = "**Q: Who is this for?**"
      , answer = [ Paragraph "A: International Educated Registered Nurses (RN) with clinical experience and a willingness to relocate to the US permanently to work as a RN in a healthcare facility setting." ]
      }
    , { question = "**Q: Is Flint a staffing company?**"
      , answer = [ Paragraph "A: Flint is an immigration and recruiting service. We are different from a typical staffing agency because we don’t just try to fill gaps in employment, we try to match the skills of the nurse with the right employer. What we really want is for nurses to be successful and ideally find a mutually beneficial long-term fit. This is in the healthcare facilities interest as well as they want to retain their staff." ]
      }
    , { question = "**Q: What do I need to apply?**"
      , answer =
            [ Paragraph "To apply you need:"
            , ListItem "A valid Registered Nursing license in any country"
            , ListItem "A 4-year Bachelors of Science in Nursing as an RN"
            , ListItem "At least 1 year of Registered Nursing clinical experience (Post-graduation)"
            , ListItem "Good English and communication skills"
            ]
      }
    , { question = "**Q: What will help my chances of being selected?**"
      , answer = [ Paragraph "A: Communication is really important for the healthcare facilities, the more effective your communication, the better. One year of clinical experience is great and 3 years is better. Moreover, nurses having a great attitude and critical thinking ability is extremely desirable." ]
      }
    , { question = "**Q: Do I have to sign a contract with Flint?**"
      , answer = [ Paragraph "A: Yes, the agreement is a two-way commitment. It legally binds us to follow through with our financial obligations and that both of us will follow through with the process." ]
      }
    , { question = "**Q: How much information about Flint will I get before making a decision?**"
      , answer = [ Paragraph "A: You can make a decision to work with us whenever you feel comfortable. We have helpful resources on our website to answer any questions you might have before making decisions. You can also call us to speak with a Flint representative." ]
      }
    , { question = "**Q: What if I am already in a longterm employment contract in my home country?**"
      , answer = [ Paragraph "A: We will consider you for a position when your contract is closer to completion. Please let your Flint representative know about this when you apply." ]
      }
    , { question = "**Q: How do I know Flint will follow through with the process?**"
      , answer = [ Paragraph "A: Early on Flint invests in your success by financing your NCLEX education and licensing. Therefore we both have interest to follow through to complete the process. Before starting we will sign agreements with you that legally binds us to our obligations." ]
      }
    , { question = "**Q: How much experience does the Flint team have?**"
      , answer = [ Paragraph "A: Our team collectively has 20+ years of immigration industry experience placing hundreds in 40+ states. We have experience with many different visas and situations." ]
      }
    , { question = "**Q: What are the working conditions for an immigrant nurse in the US?**"
      , answer = [ Paragraph "A: The working conditions for an immigrant are the same as every American nurse in the US. Some of these benefits include paid overtime, paid work breaks, and paid vacation every year." ]
      }
    , { question = "**Q: Where in the US will I work?**"
      , answer = [ Paragraph "A: We work with many different types of healthcare facilities and health centers all over the US that are looking for nurses. You will go through a matching and interview process with prospective employers before we start immigration so you know where you will be going. We consider your skills, experience, and goals when assessing which facility is best suited to you for many years. We consider both your preferences and the needs of the  to ensure that we make a match that works well for nurses and the facilities." ]
      }
    , { question = "**Q: What will be my wage?**"
      , answer = [ Paragraph "A: Nursing is a respected area of employment in the US and nurses are compensated well above the national average. Furthermore, protected by law you will be paid the same as an American nurse. You can learn more about these laws [here](https://www.dol.gov/agencies/eta/foreign-labor/wages). Your specific wage will depend on your position, location and employer. According to the [US Bureau of Labor Statistics](https://www.bls.gov/ooh/healthcare/registered-nurses.htm#tab-1), in May 2021 the median salary for an RN in the US was $77,600." ]
      }
    , { question = "**Q: How often will I be paid?**"
      , answer = [ Paragraph "A: Typically American nurses are paid every two weeks." ]
      }
    , { question = "**Q: How long is the employment contract?**"
      , answer = [ Paragraph "A: A typical employment contract will be 3 years. The goal is for us to match you with the right employer for a good long-term fit as this is what the employers want as well. We consider your skills, experience, and goals when assessing which healthcare facility is best suited to you. We consider both your preferences and the needs of the healthcare facility to ensure that we make a match that works well for nurses and the facilities. You will have the opportunity to review the contract before committing." ]
      }
    , { question = "**Q: What happens after my employment contract is complete?**"
      , answer = [ Paragraph "A: Typically the contract is renewed when the employment arrangement is a good fit." ]
      }
    , { question = "**Q: What happens when my employment arrangement doesn’t work out before finishing my three year contract?**"
      , answer = [ Paragraph "A: Typically we will help reassign you to another employer. If you have already completed more than two years with a Flint employer you will be required to work for one full year after reassignment. This is a minimum requirement from the employers." ]
      }
    , { question = "**Q: How does Flint make money?**"
      , answer = [ Paragraph "A: The healthcare facilities pay Flint for recruitment services which include finding nurses, licensing, immigration, and relocation. Flint does not charge the nurse as part of the Sponsorship Program." ]
      }
    , { question = "**Q: How much support will I get throughout the process?**"
      , answer = [ Paragraph "A: We will provide you guidance through the whole process sharing next steps along the whole journey. This includes licensing, immigration, and employment arrangements. We also connect you with a network of nurses for support. This network includes nurses who are currently going through the same step of the journey as you and international nurses who are already working in the US." ]
      }
    , { question = "**Q: Can I bring my family with me?**"
      , answer = [ Paragraph "A: Yes, you will have the opportunity for you to bring spouses and dependents under the age of 21 with you to begin your new life in the US. The costs associated with immigrating and relocating family members are not included in our service. However, we do offer additional financial assistance in the form of a loan to be paid off after you start working. If you want to immigrate with your family make sure to include that in your profile when you are prompted." ]
      }
    , { question = "**Q: Can my spouse work in the US?**"
      , answer = [ Paragraph "A: Yes, if you have a spouse relocating with you they will also have a green card which means they are authorized to work permanently in the US same as you." ]
      }
    , { question = "**Q: What will my living arrangements be in the US?**"
      , answer = [ Paragraph "A: Flint will find you a 1-year rental accommodation. This is a typical living arrangement for Americans. We will provide the first month's rent as part of your relocation package." ]
      }
    , { question = "**Q: Do I start working right after landing in the US?**"
      , answer = [ Paragraph "A: You can expect to begin to work at your healthcare facility between one month to four months depending on the requirements of your employer and the government agencies processing your file. This delay generally gives you time to get settled and organized before you start working." ]
      }
    , { question = "**Q: Should I save money in advance?**"
      , answer = [ Paragraph "A: You will have some living expenses when you arrive in the US before you receive your first paycheck. If you aren’t in a position to save, we do offer additional financial assistance in the form of a loan to be paid off after you start working." ]
      }
    , { question = "**Q: How much support will I get after landing in the US?**"
      , answer = [ Paragraph "A: To help you transition to your new country we also have a relocation advisor to help advise you on matters such as obtaining a driver's license, bank account etc." ]
      }
    , { question = "**Q: How long is the process?**"
      , answer = [ Paragraph "A: The process length varies depending on the person. Typically the process takes between 6 months and two years because we are dependent on the visa processing times." ]
      }
    , { question = "**Q: Can I stop the process with Flint?**"
      , answer = [ Paragraph "A: We really hope you stay but if you must leave you will need to repay Flint for the costs we have already paid for on your behalf." ]
      }
    , { question = "**Q: What can help speed up the process?**"
      , answer = [ Paragraph "A: The quicker you complete the steps in your online profile the faster the process will be. If you already have a US nursing license or some of the documents required for US licensing that will help speed the process." ]
      }
    , { question = "**Q: What will be my status in the US?**"
      , answer = [ Paragraph "A: You will be a permanent resident and have a green card. As a permanent resident, you will have all the same rights as all the other green card holders. In some cases the process can be supplemented with temporary visas to get you working in the US sooner." ]
      }
    , { question = "**Q: What is the NCLEX exam?**"
      , answer = [ Paragraph "A: In order to become licensed to work in the US as a RN, nurses must take and pass a qualifying exam called the NCLEX. In most cases, you must pass the NCLEX qualifying exam before we can start the immigration process. If you’ve already passed it you won’t need to take it again." ]
      }
    , { question = "**Q: How should I prepare for the NCLEX?**"
      , answer = [ Paragraph "A: Flint provides an extensive NCLEX course specifically designed to help internationally educated nurses like you prepare for the qualifying exam. As part of the program we have a Nurse Educator to prepare materials, help advise on learning strategies, and answer NCLEX related questions. Flint will also provides the opportunity connect you with other nurses who are preparing for the test for support through a discussion board." ]
      }
    , { question = "**Q: What is the NCLEX Scholarship?**"
      , answer = [ Paragraph "A: Eligible applicants receive a stipend to financially support them as they dedicate their full time to prepare for the NCLEX. Ask your Flint advisor for more information." ]
      }
    , { question = "**Q: What happens if I fail the NCLEX on my first attempt?**"
      , answer = [ Paragraph "A: Failed tests will prolong the process so it’s important to do your best to pass the first time. You may take the test again in 45 days at the earliest. This is a rule set by the testing authority." ]
      }
    , { question = "**Q: If I’m already in the immigration and licensing process can I work with Flint?**"
      , answer = [ Paragraph "A: We usually can work with you. Reach out to our team to learn more." ]
      }
    , { question = "**Q: What is the level of English proficiency that is required?**"
      , answer = [ Paragraph "A: Employers care that you can communicate effectively with staff and patients. They require you to have a professional level of English, which can be demonstrated in the interviews and in the English test." ]
      }
    , { question = "**Q: Will I need to take an English test?**"
      , answer = [ Paragraph "A: Yes, for immigration and licensing you will need to take one of either the IELTS or TOEFL English tests unless your nursing education was in the following countries and your textbooks were in English (United Kingdom, Australia, Canada (except Quebec), New Zealand, Ireland or the United States). If you have already taken an English test recently, we can use those scores." ]
      }
    , { question = "**Q: What is a passing score for the English test?**"
      , answer = [ Paragraph "A: For the IELTS (Academic Format) the overall score must be at least 6.5 and the spoken band 7.0. For the TOEFL iBT you must have an overall score greater than 83 and 26 in the speaking section." ]
      }
    , { question = "**Q: Will I need a passport?**"
      , answer = [ Paragraph "A: Yes, you and every family member coming with you will need a passport." ]
      }
    , { question = "**Q: Will I be able to study and continue to specialize in the US?**"
      , answer = [ Paragraph "A: The US has lots of programs to help you achieve your career goals. Be sure to let your prospective employer know what your goals are." ]
      }
    , { question = "**Q: Should I be concerned about discrimination in the US?**"
      , answer = [ Paragraph "A: American cities are quite multicultural where many different ethnic communities can be found. There are many opportunities to find a community that you can relate to. US laws are designed to protect individuals from discrimination. If you feel like you are being discriminated against, there are resources you can use to report these incidents. Take a look at our resources section of our website for more information." ]
      }
    , { question = "**Q: Do you employ positions other than Registered Nurses (RNs)?**"
      , answer = [ Paragraph "A: Currently we only employ RNs." ]
      }
    , { question = "**Q: What will Flint pay for?**"
      , answer = [ Paragraph "A: Flint provides a Sponsorship Program for the nurse. This includes covering expenses associated with: visa filing fees, lawyer fees, NCLEX registration, NCLEX preparation resources and materials, travel and accommodation costs associated with taking the NCLEX (varies depending on proximity to testing centers), Language test registration, Credential evaluation reports, Nursing license application, travel to the US, and initial accommodations in the US. These expenses can total to more than $20,000. Flint doesn’t require the nurse is to pay Flint back for any of these expenses after relocation." ]
      }
    , { question = "**Q: What will I need to pay for as a nurse?**"
      , answer = [ Paragraph "A: There are some services essential to immigration that you may need to pay for yourself in your home country. These services include: background check, medical examination, courier fees, and fees associated with obtaining records from both the institutions where you received your education and from your licensing authorities. The breakdown of these fees will be communicated to you before you need to make a decision. If you need help paying for expenses like these please see our Financial Assistance Program." ]
      }
    , { question = "**Q: Will I need to pay Flint money?**"
      , answer = [ Paragraph "A: No, Flint will not ask you for money directly. In addition, Flint's Sponsorship Program doesn’t require repayment from the nurse." ]
      }
    , { question = "**Q: What is the Flint Financial Assistance Program?**"
      , answer = [ Paragraph "A: This is different from the Sponsorship Program. Flint also provides an optional Financial Assistance Program in the form of a loan to be paid off after you start working. You may participate in the program to assist you with costs like initial living expenses in the US before your first paycheck. See our website for expenses Flint will cover as part of our service and to learn more about the Financial Assistance Program." ]
      }
    , { question = "**Q: What does the process with Flint look like?**"
      , answer =
            [ Paragraph "A complete outline of the process is available in your online Flint account, which you can access after you apply. Here is an overview:"
            , OrderedItem ( 1, "Apply: You can apply from the Flint website." )
            , OrderedItem ( 2, "Welcome Meeting: Meet with a Flint representative face to face to help understand your situation and how Flint can help." )
            , OrderedItem ( 3, "Complete your profile: You will be asked to submit information and necessary documents to determine how qualified you are for employment, immigration, and licensing." )
            , OrderedItem ( 4, "Flint Consultation: Meet with Flint again to discuss your application, prepare you for interviewing, and answer any questions you may have." )
            , OrderedItem ( 5, "Clinical Interview: Flint will assess your clinical skills from a US healthcare professional." )
            , OrderedItem ( 6, "Application Approval: Flint approves your application and offers a contract to walk alongside and sponsor your journey." )
            , OrderedItem ( 7, "NCLEX Course: This program is specifically designed to help internationally educated nurses like you prepare for the NCLEX qualifying exam." )
            , OrderedItem ( 8, "NCLEX Exam: Take and pass the NCLEX exam at your nearest testing center." )
            , OrderedItem ( 9, "Obtain License: After you pass the NCLEX Flint will get your US license." )
            , OrderedItem ( 10, "Match with Employer: Flint will help match you with your new US employer Your going to get invited to interview with healthcare facilities. From those interviews you will receive job offers, of which you will pick one which you will work for." )
            , OrderedItem ( 11, "Relocation: Flint manages all the visa paperwork and immigration processing on your behalf as well as travel and other preparations to start your new life in the US." )
            ]
      }
    ]
        |> List.indexedMap (\id faq -> Faq id faq.question faq.answer)
