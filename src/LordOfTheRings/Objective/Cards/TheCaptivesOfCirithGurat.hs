module LordOfTheRings.Objective.Cards.TheCaptivesOfCirithGurat (theCaptivesOfCirithGurat) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted

newtype TheCaptivesOfCirithGurat = TheCaptivesOfCirithGurat ObjectiveAttrs
  deriving anyclass (IsObjective, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

{- | While a character is attached to an encounter card, it is considered to be a guarded
objective with a blank text box: 'GuardCharacter' makes it one. Forced: When an objective
is free of encounters, it is 'rescued' and that character's owner takes control of it,
exhausted: the engine rescues a card's captives as it leaves play, an ally exhausted; a
hero this card exhausts.
-}
theCaptivesOfCirithGurat :: ObjectiveCard TheCaptivesOfCirithGurat
theCaptivesOfCirithGurat = objective TheCaptivesOfCirithGurat Cards.theCaptivesOfCirithGurat

-- | Forced: When a character is attached to an encounter card, discard each token and
-- attachment on that character.
instance RunMessage TheCaptivesOfCirithGurat where
  runMessage msg o@(TheCaptivesOfCirithGurat attrs) = case msg of
    GuardCharacter c _ -> do
      traverse_ (discard attrs) =<< getAttachmentsOn c
      damage <- getDamage c
      when (damage > 0) $ removeTokens attrs c Damage damage
      -- only a hero has a resource pool
      for_ [hid | HeroTarget hid <- [c]] \hid -> do
        resources <- (.resources) <$> getHero hid
        when (resources > 0) $ removeTokens attrs c Resource resources
      pure o
    RescueCharacter c@(HeroTarget _) -> do
      exhaust c
      pure o
    _ -> TheCaptivesOfCirithGurat <$> runMessage msg attrs
