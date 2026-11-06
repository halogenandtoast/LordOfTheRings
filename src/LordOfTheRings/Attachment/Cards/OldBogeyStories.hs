module LordOfTheRings.Attachment.Cards.OldBogeyStories (oldBogeyStories) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Helpers.Modifiers (getModifiers)

newtype OldBogeyStories = OldBogeyStories AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

oldBogeyStories :: AttachmentCard OldBogeyStories
oldBogeyStories = attachment OldBogeyStories Cards.oldBogeyStories & attachTo (AttachToHero AnyHero)

instance HasAbilities OldBogeyStories where
  getAbilities (OldBogeyStories a) =
    [ restricted a 1
        (PlayerExists (PlayerWithId a.controller <> PlayerWithCardsInHand 6) <> TargetLacksModifier (toTarget a) CannotLeavePlay)
        $ ActionAbility Free
    ]

instance RunMessage OldBogeyStories where
  runMessage msg s@(OldBogeyStories a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      player <- getPlayer pid
      mods <- getModifiers a
      when (length player.hand >= 6 && CannotLeavePlay `notElem` mods) do
        addToVictoryDisplay a
        whenInVictoryDisplay a.cardId do
          shuffleHandIntoDeck pid
          drawCards a pid 6
      pure s
    _ -> OldBogeyStories <$> runMessage msg a
