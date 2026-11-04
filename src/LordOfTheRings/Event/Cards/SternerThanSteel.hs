module LordOfTheRings.Event.Cards.SternerThanSteel (sternerThanSteel) where

import LordOfTheRings.Attachment.Types (attachmentExhausted)
import LordOfTheRings.Card (cdCardTraits, toCardDef)
import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Trait (Trait (Armor, Weapon))

newtype SternerThanSteel = SternerThanSteel EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

{- | Response: Exhaust a Weapon or Armor card attached to the defending character to cancel
a shadow effect just triggered during combat. The cards on a character are controlled by
its controller (FAQ 1.38): only he can exhaust them to pay.
-}
sternerThanSteel :: EventCard SternerThanSteel
sternerThanSteel =
  eventWith SternerThanSteel Cards.sternerThanSteel \a ->
    let you = PlayerWithId a.owner
        armed trait =
          [ HeroExists (DefendingHero <> HeroControlledBy you <> HeroWithReadyAttached trait)
          , AllyExists (DefendingAlly <> AllyControlledBy you <> AllyWithReadyAttached (Just trait))
          ]
     in a
          & (windowL ?~ ShadowEffectWindow When)
          & (criteriaL .~ (shadowEffectsCancelable <> AnyCriteria (armed Weapon <> armed Armor)))

instance RunMessage SternerThanSteel where
  runMessage msg e@(SternerThanSteel attrs) = case msg of
    PlayEvent pid card windows | card.id == toId attrs -> do
      let you = PlayerWithId pid
      defenders <- selectCharacters (DefendingHero <> HeroControlledBy you) (DefendingAlly <> AllyControlledBy you)
      attached <- concat <$> traverse playerAttachmentsOn defenders
      let gear =
            [ toTarget att
            | att <- attached
            , not (attachmentExhausted $ toAttrs att)
            , any (`member` cdCardTraits (toCardDef att)) [Weapon, Armor]
            ]
      chooseOrRunOneM pid do
        questionLabeled "sternerThanSteel.exhaust"
        causedBy attrs
        targets gear exhaust
      cancelShadowEffects windows
      pure e
    _ -> SternerThanSteel <$> runMessage msg attrs
