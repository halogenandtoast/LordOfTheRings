module LordOfTheRings.Attachment.Cards.WeatherStainedCloak (weatherStainedCloak) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Ranger))

newtype WeatherStainedCloak = WeatherStainedCloak AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a Ranger character. Limit 1 per character, on its card def.
weatherStainedCloak :: AttachmentCard WeatherStainedCloak
weatherStainedCloak =
  attachment WeatherStainedCloak Cards.weatherStainedCloak
    & attachTo (AttachToCharacterMatching (HeroWithTrait Ranger) (AllyWithTrait Ranger))

-- | While attached character is committed to the quest, Weather-stained Cloak gains:
-- "Response: Exhaust Weather-stained Cloak to cancel 1 point of damage just dealt to
-- attached character."
instance HasAbilities WeatherStainedCloak where
  getAbilities (WeatherStainedCloak a) =
    [ restricted a 1 committed $ ResponseAbility (DealtDamageWindow After host) (exhaustCost a)
    | AttachedTo host <- [a.placement]
    , committed <- case host of
        HeroTarget hid -> [exists $ HeroWithId hid <> CommittedHero]
        AllyTarget aid -> [exists $ AllyWithId aid <> CommittedAlly]
        _ -> []
    ]

instance RunMessage WeatherStainedCloak where
  -- the damage is on the character, its destruction not yet checked: the canceled point
  -- comes off before it is, as Gondorian Discipline's
  runMessage msg s@(WeatherStainedCloak a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      withHost a \host -> removeTokens a host Damage 1
      pure s
    _ -> WeatherStainedCloak <$> runMessage msg a
