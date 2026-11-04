module LordOfTheRings.Hero.Cards.FaramirLeadership (faramirLeadership) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype FaramirLeadership = FaramirLeadership HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Ranged.
faramirLeadership :: HeroCard FaramirLeadership
faramirLeadership = hero FaramirLeadership Cards.faramirLeadership

-- | Response: After you engage an enemy, ready an ally you control. (Limit once per
-- phase.)
instance HasAbilities FaramirLeadership where
  getAbilities (FaramirLeadership a) =
    [ limited (PlayerLimit PerPhase 1)
        $ restricted a 1 (exists $ AllyControlledBy (PlayerWithId a.owner) <> ExhaustedAlly)
        $ response
        $ PlayerEngagedEnemyWindow After a.owner
    ]

instance RunMessage FaramirLeadership where
  runMessage msg h@(FaramirLeadership attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      allies <- select (AllyControlledBy (PlayerWithId pid) <> ExhaustedAlly)
      chooseOrRunOneM pid do
        questionLabeled "faramir.chooseAllyToReady"
        causedBy attrs
        targets allies ready
      pure h
    _ -> FaramirLeadership <$> runMessage msg attrs
