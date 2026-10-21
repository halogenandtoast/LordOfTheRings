module LordOfTheRings.Ally.Cards.Elfhelm (elfhelm) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype Elfhelm = Elfhelm AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

elfhelm :: AllyCard Elfhelm
elfhelm = ally Elfhelm Cards.elfhelm

-- | While Elfhelm is ready, he gains: "Response: After your threat is raised as the result
-- of questing unsuccessfully, or by an encounter or quest card effect, reduce your threat
-- by 1."
--
-- ponytail: only questing unsuccessfully opens the window; threat raised by encounter
-- cards does not say where it came from yet.
instance HasAbilities Elfhelm where
  getAbilities (Elfhelm a) = [restricted a 1 (exists $ AllyWithId a.id <> ReadyAlly) $ response $ QuestedUnsuccessfullyWindow After]

instance RunMessage Elfhelm where
  runMessage msg s@(Elfhelm a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      lowerThreat pid 1
      pure s
    _ -> Elfhelm <$> runMessage msg a
