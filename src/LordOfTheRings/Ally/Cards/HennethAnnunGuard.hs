module LordOfTheRings.Ally.Cards.HennethAnnunGuard (hennethAnnunGuard) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Doomed, Sentinel))
import LordOfTheRings.Placement (Placement (Unplaced))

newtype HennethAnnunGuard = HennethAnnunGuard AllyAttrs
  deriving anyclass IsAlly
  deriving newtype (Show, Eq, ToJSON, Entity)

hennethAnnunGuard :: AllyCard HennethAnnunGuard
hennethAnnunGuard = ally HennethAnnunGuard Cards.hennethAnnunGuard

-- | You may give it doomed 1 when you play it from your hand. Asked while it is paid for.
instance HasModifiersFor HennethAnnunGuard where
  getModifiersFor (HennethAnnunGuard a) = when (a.placement == Unplaced) $ modified a (CardIdTarget a.allyCardId) [MayGainDoomed 1]

-- | If you do, it gains: "Response: After you play Henneth Annûn Guard, choose a character. Until the end of the
-- round, that character gets +2 defense and gains sentinel."
instance HasAbilities HennethAnnunGuard where
  getAbilities (HennethAnnunGuard a) =
    [ restricted a 1 (TargetHasModifier (CardIdTarget a.allyCardId) (GainKeyword (Doomed 1)))
        $ response
        $ PlayedFromHandWindow After (CardIdTarget a.allyCardId)
    ]

instance RunMessage HennethAnnunGuard where
  runMessage msg s@(HennethAnnunGuard a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      characters <- selectCharacters AnyHero AnyAlly
      chooseOneM pid do
        questionLabeled "chooseCharacter"
        causedBy a
        targets characters \c -> roundModifiers a c [DefenseModifier 2, GainKeyword Sentinel]
      pure s
    _ -> HennethAnnunGuard <$> runMessage msg a
