module LordOfTheRings.Hero.Cards.FrodoBagginsLeadership (frodoBagginsLeadership) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype FrodoBagginsLeadership = FrodoBagginsLeadership HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

frodoBagginsLeadership :: HeroCard FrodoBagginsLeadership
frodoBagginsLeadership = hero FrodoBagginsLeadership Cards.frodoBagginsLeadership

-- | Another unique character committed to the quest, there to be readied: each hero is
-- unique.
otherHeroes :: HeroAttrs -> HeroMatcher
otherHeroes a = ExhaustedHero <> CommittedHero <> not_ (HeroWithId a.id)

uniqueAllies :: AllyMatcher
uniqueAllies = ExhaustedAlly <> CommittedAlly <> UniqueAlly

-- | Response: After Frodo Baggins commits to the quest, spend 1 resource from his
-- resource pool to ready another unique character committed to the quest. If you quest
-- successfully this phase, reduce your threat by 1.
--
-- The threat reduction is a delayed effect the game resolves as the players quest
-- successfully, whether or not Frodo is still in play by then.
instance HasAbilities FrodoBagginsLeadership where
  getAbilities (FrodoBagginsLeadership a) =
    [ restricted a 1 (CharactersAtLeast 1 (otherHeroes a) uniqueAllies)
        $ ResponseAbility (CommittedToQuestWindow After (toTarget a)) (ResourceCost (toTarget a) 1)
    ]

instance RunMessage FrodoBagginsLeadership where
  runMessage msg h@(FrodoBagginsLeadership attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      characters <- selectCharacters (otherHeroes attrs) uniqueAllies
      chooseOrRunOneM pid do
        questionLabeled "frodoBagginsLeadership.ready"
        causedBy attrs
        targets characters ready
      phaseModifier attrs (PlayerTarget pid) (LowerThreatIfQuestSuccessful 1)
      pure h
    _ -> FrodoBagginsLeadership <$> runMessage msg attrs
