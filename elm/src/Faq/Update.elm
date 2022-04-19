module Faq.Update exposing (..)

import Faq.Types exposing (Model, Msg(..))
import Return exposing (Return, singleton)


init : Model
init =
    { title = "FAQ for Internationally Educated Nurses"
    , selectedTopic = 0
    , faqs =
        [ ( "Who is this for?", "International educated Registered Nurses (RN) with a willingness to relocate to the US permanently to work as a RN." )
        , ( "How does it work?", "We work with US employers such as hospitals to source great nurses internationally. We work with these international nurses getting them licensed, arranging a new job and immigration to ultimately begin their new life in the US." )
        , ( "How do I apply?", "To get started you can visit withflint.com and click the apply button." )
        , ( "What do I need to apply?", "To apply you need:\n- A valid nursing license in any country\n- Nursing education from a program approved by the U.S. State Nursing Board\n- At least 1 year of Registered Nursing experience in a hospital setting\n- Good english and communication skills" )
        , ( "Do I have to sign a contract with Flint?", "Yes, the agreement is a two way commitment. It legally binds us to follow through with our financial obligations and to follow through with the process. The agreement also ensures that you will follow through with the process as we will be investing significant financial resources into you." )
        , ( "How do I know I can trust Flint?", "We provide you the opportunity to connect with our other customers who have gone before you and are working in the US today as RNs. Before starting we will sign agreements with you which legally binds us to our obligations. We are a registered US company and are subject to all the immigration and labor laws." )
        , ( "What will Flint pay for and what do I need to pay for?", "We pay for virtually everything except small initial expenses. We need to ensure that the candidate will take this seriously and follow through. For example, we may ask you to cover the fees associated with registering for the NCLEX qualifying exam." )
        , ( "How does Flint make money?", "The US employer funds the whole process and we take a portion of that for our services." )
        , ( "Can I bring my family with me?", "Yes there are opportunities for you to bring spouses and dependents with you to begin your new life in the US. The opportunity varies depending on the immigration path you take. If you want to immigrate with your family make sure to include that in your profile when you are prompted." )
        , ( "What will my living arrangements be in the US.", "We do provide temporary accommodations such as a hotel or Airbnb for the first two weeks in the US or until your first paycheck. We help you find long term rental living accommodations just like many other Americans who cannot afford to buy a house." )
        , ( "How will I be paid?", "You will be paid every two weeks from your employer unless otherwise stated in your contract." )
        , ( "Should I save money in advance?", "It is recommended to save money in order to cover some expenses before you receive your first paycheck. We do cover basic expenses for you such as temporary accommodations and food in your first two weeks in the US or until your first paycheck." )
        , ( "How much support will I get throughout the process?", "We will provide you guidance through the whole process sharing next steps along the whole journey. This includes licensing, immigration, and employment arrangements. We connect you with other nurses who are at a similar part of the journey in your cohort to support and encourage each other. We also provide you the opportunity to connect with other nurses who have gone before you." )
        , ( "What type of Registered Nurses (RNs) are you looking for?", "Currently looking for RNs with bedside experience for hospital positions." )
        , ( "Do you employ positions other than Registered Nurses (RNs)?", "Currently we only employ RNs." )
        , ( "What will help my chances of being selected?", "The more qualified you are the better. We prefer more experience, better communication, and more nursing education. If you already have a US nursing license or some of the documents required for US licensing that is also a benefit. In addition nursing experience in the following areas are more desirable: Intensive Care, Critical Care, Medical-surgical, Emergency" )
        , ( "How long is the process?", "The process length varies case to case and depends on many factors such as the visa and country of origin. In most cases you must be licensed before we can start the immigration process. The faster you can be licensed the quicker you can start working in the US. While we cannot guarantee immigration processing times, for a typical employment based immigration path we expect up to 2 years for the full licensing and immigration to start work in the US." )
        , ( "Where in the US will I work?", "We work with many different types of hospitals and health centers all over the US that are looking for nurses. You will go through a matching and interview process with prospective employers before we start immigration so you know where you will be going. We consider your skills, experience, and goals when assessing which hospital is best suited to you. We consider both your preferences and the needs of the hospital to ensure that we make a match that works well for nurses and the facilities." )
        , ( "How long is the employment contract and what happens when it's complete?", "A typical employment contract will be 3 years. The goal is for us to match you with the right employer for a good long term fit as this is what the employers want as well. We consider your skills, experience, and goals when assessing which hospital is best suited to you. We consider both your preferences and the needs of the hospital to ensure that we make a match that works well for nurses and the facilities. You will have opportunities to review the contract before committing." )
        , ( "What type of US visa would I be using?", "This depends on your situation. The most popular visa process is the EB3 visa which is an employment based permanent resident visa. If you are Canadian or Mexican other options include a TN visa which is a temporary work visa. By filling out your profile we help identify alternative immigration paths that may work better for your specific situation." )
        , ( "What is the NCLEX exam?", "In order to become licensed to work in the US as a RN, nurses must take and pass a qualifying exam called the NCLEX. In most cases you must pass the NCLEX qualifying exam before we can start the immigration process. If you've already passed it you won't need to take it again." )
        , ( "How should I prepare for the NCLEX?", "We provide study materials and resources to help you prepare. We also connect you with other nurses who are also preparing for the test to support each other." )
        , ( "What happens if I fail the NCLEX in my first attempt?", "Failed tests will prolong the process so it's important to do your best to pass the first time. You may take the test again in 45 days at the earliest. This is a rule set by the testing authority. " )
        , ( "If I'm already in the immigration and licensing process can I work with Flint?", "We can but it depends on the situation. If you have specific questions you can reach out to our team to find out more." )
        , ( "What is the level of English proficiency that is required?", "Employers care that you can communicate effectively with staff and patients. They require you to be fluent in English, which can be demonstrated in the interviews and in the English test." )
        , ( "Will I need to take an English test?", "Yes, for immigration and licensing you will need to take one of either the IELTS or TOEFL english tests unless your nursing education was in the following countries and your textbooks were in English (United Kingdom, Australia, Canada (except Quebec), New Zealand, Ireland or the United States). If you have already taken an English test recently, we can use those scores." )
        , ( "What is a passing score for the English test?", "For the IELTS (Academic Format) the overall score must be at least 6.5 and the spoken band 7.0. For the TOEFL iBT you must have an overall score greater than 83 and 26 in the speaking section." )
        , ( "Will I need a passport?", "Yes you and every family member coming with you will need a passport." )
        , ( "Will I be able to study and continue to specialize in certain nursing areas after I come?", "The US has lots of programs to help you achieve your career goals. Be sure to let your prospective employer know what your goals are." )
        ]
    }


update : Msg -> Model -> Return Msg Model
update msg model =
    case msg of
        Select topicId ->
            singleton { model | selectedTopic = topicId }

        Hide ->
            singleton { model | selectedTopic = -1 }
