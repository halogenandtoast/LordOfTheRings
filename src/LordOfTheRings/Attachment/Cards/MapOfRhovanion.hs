module LordOfTheRings.Attachment.Cards.MapOfRhovanion (mapOfRhovanion) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Dale, Scout))

newtype MapOfRhovanion = MapOfRhovanion AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a Dale or Scout character. Limit 1 per character.
mapOfRhovanion :: AttachmentCard MapOfRhovanion
mapOfRhovanion =
  attachment MapOfRhovanion Cards.mapOfRhovanion
    & attachTo
      ( AttachToCharacterMatching
          (HeroMatchAny [HeroWithTrait Dale, HeroWithTrait Scout])
          (AllyMatchAny [AllyWithTrait Dale, AllyWithTrait Scout])
      )

-- | Response: After attached character commits to the quest, place 1 progress on the active
-- location.
instance HasAbilities MapOfRhovanion where
  getAbilities (MapOfRhovanion a) =
    [ restricted a 1 (exists IsActiveLocation) $ response $ CommittedToQuestWindow After host
    | AttachedTo host <- [a.placement]
    ]

instance RunMessage MapOfRhovanion where
  runMessage msg s@(MapOfRhovanion a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      traverse_ (\l -> placeProgress a l 1) =<< select IsActiveLocation
      pure s
    _ -> MapOfRhovanion <$> runMessage msg a
